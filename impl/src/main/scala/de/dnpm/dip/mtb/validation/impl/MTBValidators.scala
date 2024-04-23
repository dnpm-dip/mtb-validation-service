package de.dnpm.dip.mtb.validation.impl


import scala.util.chaining._
import cats.{
  Applicative,
  Id
}
import cats.data.Ior
import cats.syntax.validated._
import de.ekut.tbi.validation.{
  Validator,
  NegatableValidator
}
import de.ekut.tbi.validation.dsl._
import de.dnpm.dip.util.DisplayLabel
import de.dnpm.dip.util.Displays
import de.dnpm.dip.coding.{
  CodedEnum,
  Coding,
  CodeSystem,
  CodeSystemProvider
}
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.icd.ICD10GM
import de.dnpm.dip.coding.icd.ICDO3
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.coding.hgvs.HGVS
import de.dnpm.dip.model.{
  ClosedInterval,
  Patient,
  Reference,
  Therapy,
  TherapyRecommendation
}
import de.dnpm.dip.service.validation.{
  HasId,
  Issue,
  Validators
}
import de.dnpm.dip.mtb.model._
import TumorCellContent.Method.{
  Bioinformatic,
  Histologic
}
import Issue.{
  Error,
  Info,
  Path,
  Warning
}
import Path.root
import Path.syntax._


trait MTBValidators extends Validators
{

  private implicit def displayEnumValue[E <: CodedEnum](
    implicit cs: CodeSystem[E#Value]
  ): Displays[E#Value] =
    Displays[E#Value](e => cs.conceptWithCode(e.toString).get.display)


  private implicit val whoGradingCsp: CodeSystemProvider[WHOGrading,Id,Applicative[Id]] =
    new WHOGrading.Provider.Facade[Id]


  implicit val performanceStatusNode: Path.Node[PerformanceStatus] =
    Path.Node("Performance-Status")

  implicit val tumorSpecimenNode: Path.Node[TumorSpecimen] =
    Path.Node("Tumor-Probe")

  implicit val tumorCellContentNode: Path.Node[TumorCellContent] =
    Path.Node("Tumor-Zellgehalt")

  implicit val tumorMorphologyNode: Path.Node[TumorMorphology] =
    Path.Node("Tumor-Morphologie")

  implicit val histologyReportNode: Path.Node[HistologyReport] =
    Path.Node("Histologie-Bericht")

  implicit val ngsReportNode: Path.Node[NGSReport] =
    Path.Node("NGS-Bericht")

  implicit val tmbNode: Path.Node[TMB] =
    Path.Node("TMB-Befund")

  implicit val brcanessNode: Path.Node[BRCAness] =
    Path.Node("BRCAness")

  implicit val hrdScoreNode: Path.Node[HRDScore] =
    Path.Node("HRD-Score")

  implicit val variantNode: Path.Node[Variant] =
    Path.Node("Variante")

  implicit val snvNode: Path.Node[SNV] =
    Path.Node("SNV")

  implicit val cnvNode: Path.Node[CNV] =
    Path.Node("CNV")

  implicit val dnaFusionNode: Path.Node[DNAFusion] =
    Path.Node("DNA-Fusion")

  implicit val rnaFusionNode: Path.Node[RNAFusion] =
    Path.Node("RNA-Fusion")

  implicit val mtbMedicationRecommendationNode: Path.Node[MTBMedicationRecommendation] =
    Path.Node("MTB-Therapie-Empfehlung")

  implicit val carePlanNode: Path.Node[MTBCarePlan] =
    Path.Node("MTB-Beschluss")

  implicit val claimNode: Path.Node[Claim] =
    Path.Node("Kostenübernahme-Antrag")

  implicit val claimResponseNode: Path.Node[ClaimResponse] =
    Path.Node("Kostenübernahme-Antwort")


  implicit val icd10gm: CodeSystemProvider[ICD10GM,Id,Applicative[Id]]

  implicit val icdo3: ICDO3.Catalogs[Id,Applicative[Id]]

  implicit val atc: CodeSystemProvider[ATC,Id,Applicative[Id]]

  implicit val hgnc: CodeSystemProvider[HGNC,Id,Applicative[Id]] 

  implicit lazy val icdo3TCodingValidator: Validator[Issue.Builder,Coding[ICDO3.T]] = {
    coding =>

      val icdo3t =
        coding.version
          .flatMap(icdo3.topography(_))
          .getOrElse(icdo3.topography)

      csCodingValidator(icdo3t)(coding)
  }

  implicit lazy val icdo3MCodingValidator: Validator[Issue.Builder,Coding[ICDO3.M]] = {
    coding =>

      val icdo3m =
        coding.version
          .flatMap(icdo3.morphology(_))
          .getOrElse(icdo3.morphology)

      csCodingValidator(icdo3m)(coding)
  }


  implicit def diagnosisValidator(
    implicit
    patient: Patient,
  ): Validator[Issue,MTBDiagnosis] =
    diagnosis =>
      (
        validate(diagnosis.patient) at "Patient",
        validate(diagnosis.code) at "Code",
        diagnosis.topography must be (defined) otherwise (
          Info("Fehlende optionale Angabe, ggf. nachprüfen")
        ) andThen (validateOpt(_)) at "Topographie",
        validateOpt(diagnosis.whoGrading) at "WHO-Graduierung",
        diagnosis.stageHistory must be (defined) otherwise (
          Warning("Fehlende Angabe") at "Tumor-Ausbreitungsstadium"
        ),
        diagnosis.guidelineTreatmentStatus must be (defined) otherwise (
          Warning("Fehlende Angabe") at "Leitlinien-Behandlungsstatus"
        )
      )
      .errorsOr(diagnosis) on diagnosis


  def GuidelineTherapyValidator(
    implicit
    patient: Patient,
    diagnoses: Iterable[MTBDiagnosis],
    recommendations: Iterable[MTBMedicationRecommendation],
  ): Validator[Issue,MTBMedicationTherapy] =
    TherapyValidator[MTBMedicationTherapy] combineWith (
      therapy =>
        therapy.medication must be (defined) otherwise (
           Warning("Fehlende Angabe") at "Medikation"
        ) map (_.get.toList) andThen (
          validateEach(_) at "Medikation"
        ) map (_ => therapy) on therapy
      )


  import Therapy.Status.{Ongoing,Completed,Stopped}

  def MTBTherapyValidator(
    implicit
    patient: Patient,
    diagnoses: Iterable[MTBDiagnosis],
    recommendations: Iterable[MTBMedicationRecommendation],
  ): Validator[Issue,MTBMedicationTherapy] =
    TherapyValidator[MTBMedicationTherapy] combineWith (
      therapy =>
        (
          therapy.statusValue match {
            case Ongoing | Completed | Stopped  =>
              therapy.medication.getOrElse(Set.empty).toList must be (nonEmpty) otherwise (
                Error("Fehlende Angabe bei begonnener Therapie")
              ) andThen (
                validateEach(_)
              ) at "Medikation"
            case _ => None.validNel[Issue]
          },
          therapy.statusValue match {
            case Ongoing =>
              therapy.period must be (defined) otherwise (
                Error("Fehlende Angabe bei begonnener Therapie") at "Zeitraum"
              )
            case Completed | Stopped =>
              therapy.period must be (defined) otherwise (
                Error("Fehlende Angabe bei begonnener Therapie") at "Zeitraum"
              ) andThen (
                _.get.endOption must be (defined) otherwise (
                  Error("Fehlende Angabe bei abgeschlossener Therapie") at "End-Datum"
                ) on "Zeitraum"
              )
            case _ => None.validNel[Issue]
          },
          therapy.statusReason must be (defined) otherwise (
            Warning("Fehlende Angabe") at "Status-Grund"
          )
        )
        .errorsOr(therapy) on therapy
      )


  implicit def OncoProcedureValidator(
    implicit
    patient: Patient,
    diagnoses: Iterable[MTBDiagnosis],
    recommendations: Iterable[TherapyRecommendation],
  ): Validator[Issue,OncoProcedure] =
    TherapyValidator[OncoProcedure]


  implicit def performanceStatusValidator(
    implicit
    patient: Patient
  ): Validator[Issue,PerformanceStatus] =
    ObservationValidator[PerformanceStatus]

  
  implicit def specimenValidator(
    implicit
    patient: Patient,
    diagnoses: Iterable[MTBDiagnosis],
  ): Validator[Issue,TumorSpecimen] = {
    specimen =>
      (
        validate(specimen.patient) at "Patient",
        validate(specimen.diagnosis) at "Diagnose",
        specimen.`type` must not (be (Coding(TumorSpecimen.Type.Unknown))) otherwise (
          Warning(s"Fehlende bzw. Unspezifische Angabe '${DisplayLabel.of(specimen.`type`.code).value}'") at "Proben-Typ"
        )
      )
      .errorsOr(specimen) on specimen
  }


  private def TumorCellContentValidator(
    method: TumorCellContent.Method.Value
  )(
    implicit
    patient: Patient,
    specimens: Iterable[TumorSpecimen],
  ): Validator[Issue,TumorCellContent] =
    ObservationValidator[TumorCellContent](ClosedInterval(0.0 -> 1.0))
      .combineWith {
        tcc =>
          val expectedMethod = Coding(method)
          (
            validate(tcc.specimen) at "Probe",
            tcc.method must be (expectedMethod) otherwise (
              Error(s"Ungültige Bestimmungs-Methode, '${DisplayLabel.of(expectedMethod)}' erwartet")
            ) at "Methode",
          )
          .errorsOr(tcc) on tcc
      }
  

  implicit def tumorMorphologyValidator(
    implicit
    patient: Patient,
    specimens: Iterable[TumorSpecimen],
  ): Validator[Issue,TumorMorphology] =
    ObservationValidator[TumorMorphology] combineWith (
      obs => (validate(obs.specimen) at "Probe") map (_ => obs) on obs
    )


  implicit def histologyReportValidator(
    implicit
    patient: Patient,
    specimens: Iterable[TumorSpecimen],
  ): Validator[Issue,HistologyReport] = {
     report =>

      implicit val tumorCellContentValidator =
        TumorCellContentValidator(Histologic)

      val tumorMorphology  = report.results.tumorMorphology
      val tumorCellContent = report.results.tumorCellContent

      (
        validate(report.patient) at "Patient",
        validate(report.specimen) at "Probe",
        (tumorMorphology orElse tumorCellContent) must be (defined) otherwise (
          Error("Keine Befunde vorhanden, weder Tumor-Morphologie noch -Zellgehalt") at "Ergebnisse"
        ),
        tumorMorphology must be (defined) otherwise (
          Warning("Fehlender Befund") at Path.Node[TumorMorphology].name
        ) andThen (validateOpt(_)) on "Ergebnisse",
        tumorCellContent must be (defined) otherwise (
          Warning("Fehlender Befund") at Path.Node[TumorCellContent].name
        ) andThen (
          validateOpt(_) 
        ) on "Ergebnisse"
      )
      .errorsOr(report) on report
  }


  private implicit def tmbValidator(
    implicit
    patient: Patient
  ): Validator[Issue,TMB] =
    ObservationValidator[TMB](TMB.referenceRange)


  private implicit def brcanessValidator(
    implicit
    patient: Patient
  ): Validator[Issue,BRCAness] =
    ObservationValidator[BRCAness](BRCAness.referenceRange)


  private implicit def hrdScoreValidator(
    implicit
    patient: Patient
  ): Validator[Issue,HRDScore] =
    ObservationValidator[HRDScore](HRDScore.referenceRange)


  private implicit def snvValidator(
    implicit
    patient: Patient,
  ): Validator[Issue,SNV] =
    snv =>
      (
        validate(snv.patient) at "Patient",
        validateOpt(snv.gene) at "Gen",
        ifDefined(snv.proteinChange.map(_.code.value))(
          code => code must matchRegex (HGVS.Protein.threeLetterAA) otherwise (
            Error(s"Ungültiger Code '$code', erwarte 3-Buchstaben-Format") at "Amino-Säure-Austausch"
          )
        )
      )
      .errorsOr(snv) on snv

  private implicit def cnvValidator(
    implicit
    patient: Patient,
  ): Validator[Issue,CNV] =
    cnv =>
      (
        validate(cnv.patient) at "Patient",
        validateEach(cnv.reportedAffectedGenes.getOrElse(Set.empty).toList) at "Reported Affected Genes",
        validateEach(cnv.copyNumberNeutralLoH.getOrElse(Set.empty).toList) at "CopyNumber Neutral LoH"
      )
      .errorsOr(cnv) on cnv
  
  private implicit def dnaFusionValidator(
    implicit
    patient: Patient,
  ): Validator[Issue,DNAFusion] = {
    fusion =>
      (
        validate(fusion.patient) at "Patient",
        validate(fusion.fusionPartner5prime.gene) at "Fusions-Gen 5'",
        validate(fusion.fusionPartner3prime.gene) at "Fusions-Gen 3'",
      )
      .errorsOr(fusion) on fusion
  }

  private implicit def rnaFusionValidator(
    implicit
    patient: Patient,
  ): Validator[Issue,RNAFusion] = {
    fusion =>
      (
        validate(fusion.patient) at "Patient",
        validate(fusion.fusionPartner5prime.gene) at "Fusions-Gen 5'",
        validate(fusion.fusionPartner3prime.gene) at "Fusions-Gen 3'",
      )
      .errorsOr(fusion) on fusion
  }


  implicit def ngsReportValidator(
    implicit
    patient: Patient,
    specimens: Iterable[TumorSpecimen],
  ): Validator[Issue,NGSReport] = {
    report =>
      implicit val tumorCellContentValidator =
        TumorCellContentValidator(Bioinformatic)

      (
        validate(report.patient) at "Patient",
        validate(report.specimen) at "Probe",
        report.results.tumorCellContent must be (defined) otherwise (
          Warning("Fehlender Befund") at Path.Node[TumorCellContent].name
        ) andThen (
          validateOpt(_)
        ) on "Ergebnisse",
        report.results.tmb must be (defined) otherwise (
          Warning("Fehlender Befund") at Path.Node[TMB].name
        ) map (_.get) andThen (
          validate(_)
        ) on "Ergebnisse",
        report.results.brcaness must be (defined) otherwise (
          Warning("Fehlender Befund") at Path.Node[BRCAness].name
        ) map (_.get) andThen (
          validate(_)
        ) on "Ergebnisse",
        report.results.hrdScore must be (defined) otherwise (
          Warning("Fehlender Befund") at Path.Node[HRDScore].name
        ) map (_.get) andThen (
          validate(_)
        ) on "Ergebnisse",
        report.results.simpleVariants must be (nonEmpty) otherwise (
          Warning("Fehlende Befunde") at "Einfache Varianten"
        ) andThen(
          validateEach(_)
        ) on "Ergebnisse",
        report.results.copyNumberVariants must be (nonEmpty) otherwise (
          Warning("Fehlende Befunde") at "CNVs"
        ) andThen(
          validateEach(_)
        ) on "Ergebnisse",
        report.results.dnaFusions must be (nonEmpty) otherwise (
          Info("Fehlende Befunde") at "DNA-Fusionen"
        ) andThen(
          validateEach(_)
        ) on "Ergebnisse",
        report.results.rnaFusions must be (nonEmpty) otherwise (
          Info("Fehlende Befunde") at "RNA-Fusionen"
        ) andThen(
          validateEach(_)
        ) on "Ergebnisse"

      )
      .errorsOr(report) on report
  }

  implicit def medicationRecommendationValidator(
    implicit
    patient: Patient,
    diagnoses: Iterable[MTBDiagnosis],
    variants: Iterable[Variant],
  ): Validator[Issue,MTBMedicationRecommendation] =
    rec =>
      (
        validate(rec.patient) at "Patient",
        validate(rec.indication) at "Indikation",
        rec.levelOfEvidence must be (defined) otherwise (Warning("Fehlende Angabe")) at "Evidenz-Level",
        rec.medication must be (nonEmpty) otherwise (Error("Fehlende Angabe")) at "Medikation",
        rec.supportingEvidence.getOrElse(List.empty) must be (nonEmpty) otherwise (
          Warning("Fehlende Angabe")
        ) andThen (
          validateEach(_)
        ) at "Stützende molekulare Alteration(en)",
      )
      .errorsOr(rec) on rec


  implicit def carePlanValidator(
    implicit
    patient: Patient,
    diagnoses: Iterable[MTBDiagnosis],
    variants: Iterable[Variant],
  ): Validator[Issue,MTBCarePlan] =
    carePlan =>
      (
        validate(carePlan.patient) at "Patient",
        validate(carePlan.indication) at "Indikation",
        (carePlan.medicationRecommendations.filter(_.nonEmpty) orElse carePlan.statusReason) must be (defined) otherwise (
          Error(s"Fehlende Angabe: Es müssen entweder Therapie-Empfehlungen oder explizit Status-Grund '${DisplayLabel.of(MTBCarePlan.StatusReason.NoTarget)}' aufgeführt sein")
            at "Status-Grund"
        ),
        ifDefined(carePlan.medicationRecommendations)(validateEach(_))
      )
      .errorsOr(carePlan) on carePlan


  implicit def claimValidator(
    implicit
    patient: Patient,
    recommendations: Iterable[MTBMedicationRecommendation],
    claimResponses: Iterable[ClaimResponse]
  ): Validator[Issue,Claim] =
    claim =>
      (
        validate(claim.patient) at "Patient",
        validate(claim.recommendation) at Path.Node[MTBMedicationRecommendation].name,
        claim.id must be (in (claimResponses.flatMap(_.claim.id))) otherwise (
          Warning(s"Keine zugehörige ${Path.Node[ClaimResponse].name} vorhanden") at root
        ) 
      )
      .errorsOr(claim) on claim


  implicit def claimResponseValidator(
    implicit
    patient: Patient,
    claims: Iterable[Claim]
  ): Validator[Issue,ClaimResponse] =
    response =>
      (
        validate(response.patient) at "Patient",
        validate(response.claim) at Path.Node[Claim].name,
        response.statusReason must be (defined) otherwise (Warning(s"Fehlende Angabe")) at "Status-Grund"
      )
      .errorsOr(response) on response


  val patientRecordValidator: Validator[Issue,MTBPatientRecord] = {
    record =>

      implicit val patient =
        record.patient

      implicit val diagnoses = 
        record.getDiagnoses

      implicit val recommendations =
        record.getCarePlans
          .flatMap(_.medicationRecommendations.getOrElse(List.empty))

      implicit val specimens = 
        record.getSpecimens

      implicit val variants = 
        record.getNgsReports.flatMap(_.variants)

      implicit val claims =
        record.getClaims

      implicit val claimResponses =
        record.getClaimResponses


      (
        validate(record.patient),
        diagnoses must be (nonEmpty) otherwise (
          Error(s"Fehlende Angabe") at "Diagnosen"
        ) andThen (
          validateEach(_)
        ),        
        record.getGuidelineMedicationTherapies must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at "Leitlinien-Therapien"
        ) andThen {
          implicit val v = GuidelineTherapyValidator
          validateEach(_)
        },
        record.getGuidelineProcedures must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at "Leitlinien-Prozeduren"
        ) andThen (
          validateEach(_)
        ),
        record.getPerformanceStatus must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at "Performance-Status"
        ) andThen (
          validateEach(_)
        ),
        specimens must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at "Tumor-Proben"
        ) andThen (
          validateEach(_)
        ),
        record.getHistologyReports must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at "Histologie-Berichte"
        ) andThen (
          validateEach(_)
        ),
        //TODO: IHC-Reports
        record.getNgsReports must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at "NGS-Berichte"
        ) andThen (
          validateEach(_)
        ),
        record.getCarePlans must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at "MTB-Beschlüsse"
        ) andThen (
          validateEach(_)
        ),
        claims must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at "Kostenübernahme-Anträge"
        ) andThen (
          validateEach(_)
        ),
        claimResponses must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at "Kostenübernahme-Antworten"
        ) andThen (
          validateEach(_)
        ),
        record.getMedicationTherapies must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at "MTB-Therapien"
        ) map (_.flatMap(_.history)) andThen {
          implicit val v = MTBTherapyValidator
          validateEach(_)
        }
                
      )
      .errorsOr(record)
  }

}

object MTBValidators extends MTBValidators
{

  implicit lazy val hgnc: CodeSystemProvider[HGNC,Id,Applicative[Id]] =
    HGNC.GeneSet
      .getInstance[Id]
      .get

  implicit lazy val atc: CodeSystemProvider[ATC,Id,Applicative[Id]] =
    ATC.Catalogs
      .getInstance[Id]
      .get

  implicit lazy val icd10gm: CodeSystemProvider[ICD10GM,Id,Applicative[Id]] =
    ICD10GM.Catalogs
      .getInstance[Id]
      .get

  implicit lazy val icdo3: ICDO3.Catalogs[Id,Applicative[Id]] =
    ICDO3.Catalogs
      .getInstance[Id]
      .get

}
