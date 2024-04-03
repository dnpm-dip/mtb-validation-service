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
import de.dnpm.dip.model.{
  ClosedInterval,
  Patient,
  Therapy
}
import de.dnpm.dip.service.validation.{
  HasId,
  Issue,
  Validators
}
import de.dnpm.dip.mtb.model._
import Issue.{
  Error,
  Info,
  Path,
  Warning
}
import Path.root
import Path.syntax._


object MTBValidators extends Validators
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

//  implicit val _Node: Path.Node[] =
//    Path.Node("")

  implicit val carePlanNode: Path.Node[MTBCarePlan] =
    Path.Node("MTB-Beschluss")




  implicit def icdo3TCodingValidator(
    implicit icdo3: ICDO3.Catalogs[Id,Applicative[Id]]
  ): Validator[Issue.Builder,Coding[ICDO3.T]] = {
    coding =>

      val icdo3t =
        coding.version
          .flatMap(icdo3.topography(_))
          .getOrElse(icdo3.topography)

      csCodingValidator(icdo3t)(coding)
  }

  implicit def icdo3MCodingValidator(
    implicit icdo3: ICDO3.Catalogs[Id,Applicative[Id]]
  ): Validator[Issue.Builder,Coding[ICDO3.M]] = {
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
    icd10gm: CodeSystemProvider[ICD10GM,Id,Applicative[Id]],
    icdo3: ICDO3.Catalogs[Id,Applicative[Id]],
  ): Validator[Issue,MTBDiagnosis] =
    diagnosis =>
      (
        validate(diagnosis.patient) at diagnosis/"Patient",
        validate(diagnosis.code) at diagnosis/"Code",
        diagnosis.topography must be (defined) otherwise (
          Info("Fehlende Angabe")
        ) andThen (
          c => validate(c.get)
        ) at diagnosis/"Topographie",
        ifDefined(diagnosis.whoGrading)(validate(_) at diagnosis/"WHO-Graduierung"),
        diagnosis.guidelineTreatmentStatus must be (defined) otherwise (
          Warning("Fehlende Angabe") at diagnosis/"Leitlinien-Behandlungsstatus"
        )
      )
      .errorsOr(diagnosis)



  def GuidelineTherapyValidator(
    implicit
    patient: Patient,
    diagnoses: Iterable[MTBDiagnosis],
    recommendations: Iterable[MTBMedicationRecommendation],
    atc: CodeSystemProvider[ATC,Id,Applicative[Id]]
  ): Validator[Issue,MTBMedicationTherapy] =
    therapy =>
      (
        validate(therapy),
        therapy.medication.map(_.toList)
          .pipe(
            ifDefined(_)(validateEach(_) at therapy/"Medikation")
          )
      )
      .errorsOr(therapy)


  def MTBTherapyValidator(
    implicit
    patient: Patient,
    diagnoses: Iterable[MTBDiagnosis],
    recommendations: Iterable[MTBMedicationRecommendation],
    atc: CodeSystemProvider[ATC,Id,Applicative[Id]]
  ): Validator[Issue,MTBMedicationTherapy] = {
    therapy =>

      import Therapy.Status.{Ongoing,Completed,Stopped}

      (
        validate(therapy),
        therapy.statusValue match {
          case Ongoing | Completed | Stopped  =>
            therapy.medication.getOrElse(Set.empty).toList must be (nonEmpty) otherwise (
              Error("Fehlende Angabe bei begonnener Therapie")
            ) andThen (
              validateEach(_)
            ) at therapy/"Medikation"
          case _ => None.validNel[Issue]
        },
        therapy.statusValue match {
          case Ongoing =>
            therapy.period must be (defined) otherwise (
              Error("Fehlende Angabe bei begonnener Therapie") at therapy/"Zeitraum"
            )
          case Completed | Stopped =>
            therapy.period must be (defined) otherwise (
              Error("Fehlende Angabe bei begonnener Therapie") at therapy/"Zeitraum"
            ) andThen (
              _.get.endOption must be (defined) otherwise (
                Error("Fehlende Angabe bei abgeschlossener Therapie") at therapy/"Zeitraum"/"End-Datum"
              )
            )
            
          case _ => None.validNel[Issue]
        },
        therapy.statusReason must be (defined) otherwise (
          Warning("Fehlende Angabe") at therapy/"Status-Grund"
        )
      )
      .errorsOr(therapy)
  }


  implicit def performanceStatusValidator(
    implicit
    patient: Patient
  ): Validator[Issue,PerformanceStatus] =
    ObservationValidator[Coding[ECOG.Value],PerformanceStatus](
      csCodingValidator(ECOG.codeSystem)
    )


  
  implicit def specimenValidator(
    implicit
    patient: Patient,
    diagnoses: Iterable[MTBDiagnosis],
  ): Validator[Issue,TumorSpecimen] = {
    specimen =>
      (
        validate(specimen.patient) at specimen/"Patient",
        validate(specimen.diagnosis) at specimen/"Diagnose",
        specimen.`type` must not (be (Coding(TumorSpecimen.Type.Unknown))) otherwise (
          Warning(s"Fehlende/Unspezifische Angabe '${DisplayLabel.of(specimen.`type`.code).value}'")
        ) at specimen/"Typ"
      )
      .errorsOr(specimen)
  }


  private val tumorCellContentRange =
    ClosedInterval(0.0 -> 1.0)

  private def TumorCellContentValidator(
    method: TumorCellContent.Method.Value
  )(
    implicit
    patient: Patient,
    specimens: Iterable[TumorSpecimen],
  ): Validator[Issue,TumorCellContent] = {
    tcc =>
      val expectedMethod = Coding(method)
      (
        validate(tcc.patient) at tcc/"Patient",
        validate(tcc.specimen) at tcc/"Probe",
        tcc.method must be (expectedMethod) otherwise (
          Error(s"Ungültige Bestimmungs-Methode, '${DisplayLabel.of(expectedMethod)}' erwartet")
        ) at tcc/"Methode",
        tcc.value must be (in (tumorCellContentRange)) otherwise (
          Error(s"Ungültiger Wert ${tcc.value}, nicht in Referenz-Bereich $tumorCellContentRange")
        ) at tcc/"Wert"
      )
      .errorsOr(tcc)
  }


  implicit def tumorMorphologyValidator(
    implicit
    patient: Patient,
    specimens: Iterable[TumorSpecimen],
    icdo3: ICDO3.Catalogs[Id,Applicative[Id]]
  ): Validator[Issue,TumorMorphology] =
    obs =>
      (
        validate(obs.patient) at obs/"Patient",
        validate(obs.specimen) at obs/"Probe",
        validate(obs.value) at obs/"Wert"
      )
      .errorsOr(obs)



  implicit def histologyReportValidator(
    implicit
    patient: Patient,
    specimens: Iterable[TumorSpecimen],
    icdo3: ICDO3.Catalogs[Id,Applicative[Id]],
  ): Validator[Issue,HistologyReport] = {
     report =>

      val tumorMorphology  = report.results.tumorMorphology
      val tumorCellContent = report.results.tumorCellContent
      val expectedMethod   = Coding(TumorCellContent.Method.Histologic).code

      (
        validate(report.patient) at report/"Patient",
        validate(report.specimen) at report/"Probe",
        (tumorMorphology orElse tumorCellContent) must be (defined) otherwise (
          Error("Keine Befunde vorhanden, weder Tumor-Morphologie noch -Zellgehalt") at report/"Ergebnisse"
        ),
        tumorMorphology must be (defined) otherwise (
          Warning("Fehlender Befund") at report/"Ergebnisse"/Path.Node[TumorMorphology].name
        ) map (_.get) andThen (
          validate(_) 
        ),
        tumorCellContent must be (defined) otherwise (
          Warning("Fehlender Befund") at Path.root/Path.Node[TumorCellContent].name
        ) map (_.get) andThen (
          TumorCellContentValidator(TumorCellContent.Method.Histologic)
        ) at report/"Ergebnisse"
      )
      .errorsOr(report)
  }


  private implicit def tmbValidator(
    implicit
    patient: Patient
  ): Validator[Issue,TMB] =
    ObservationValidator[TMB.Result,TMB](
      tmb => tmb must be (in (TMB.referenceRange)) otherwise (
        Error(s"Ungültiger Wert ${tmb.value}, nicht in Referenz-Bereich ${TMB.referenceRange}")
      )
    )

  private implicit def brcanessValidator(
    implicit
    patient: Patient
  ): Validator[Issue,BRCAness] =
    ObservationValidator[Double,BRCAness](
      value => value must be (in (BRCAness.referenceRange)) otherwise (
        Error(s"Ungültiger Wert $value, nicht in Referenz-Bereich ${BRCAness.referenceRange}")
      )
    )


  private implicit def hrdScoreValidator(
    implicit
    patient: Patient
  ): Validator[Issue,HRDScore] =
    ObservationValidator[Double,HRDScore](
      value => value must be (in (HRDScore.referenceRange)) otherwise (
        Error(s"Ungültiger Wert $value, nicht in Referenz-Bereich ${HRDScore.referenceRange}")
      )
    )

  private implicit def snvValidator(
    implicit
    patient: Patient,
    geneValidator: Validator[Issue.Builder,Coding[HGNC]]
  ): Validator[Issue,SNV] =
    snv =>
      (
        validate(snv.patient) at snv/"Patient",
        ifDefined(snv.gene)(validate(_)) at snv/"Gen"
      )
      .errorsOr(snv)

  private implicit def cnvValidator(
    implicit
    patient: Patient,
    geneValidator: Validator[Issue.Builder,Coding[HGNC]]
  ): Validator[Issue,CNV] =
    cnv =>
      (
        validate(cnv.patient) at cnv/"Patient",
        validateEach(cnv.reportedAffectedGenes.toList) at cnv/"Reported Affected Genes",
        validateEach(cnv.copyNumberNeutralLoH.toList) at cnv/"CopyNumber Neutral LoH"
      )
      .errorsOr(cnv)
  
  private implicit def dnaFusionValidator(
    implicit
    patient: Patient,
    geneValidator: Validator[Issue.Builder,Coding[HGNC]]
  ): Validator[Issue,DNAFusion] = {
    fusion =>
      (
        validate(fusion.patient) at fusion/"Patient",
        validate(fusion.fusionPartner5prime.gene) at fusion/"Fusions-Gen 5'",
        validate(fusion.fusionPartner3prime.gene) at fusion/"Fusions-Gen 3'",
      )
      .errorsOr(fusion)
  }

  private implicit def rnaFusionValidator(
    implicit
    patient: Patient,
    geneValidator: Validator[Issue.Builder,Coding[HGNC]]
  ): Validator[Issue,RNAFusion] = {
    fusion =>
      (
        validate(fusion.patient) at fusion/"Patient",
        validate(fusion.fusionPartner5prime.gene) at fusion/"Fusions-Gen 5'",
        validate(fusion.fusionPartner3prime.gene) at fusion/"Fusions-Gen 3'",
      )
      .errorsOr(fusion)
  }


  implicit def ngsReportValidator(
    implicit
    patient: Patient,
    specimens: Iterable[TumorSpecimen],
    hgnc: CodeSystemProvider[HGNC,Id,Applicative[Id]]
  ): Validator[Issue,NGSReport] =
    report =>
      (
        validate(report.patient) at report/"Patient",
        validate(report.specimen) at report/"Probe",
        report.results.tumorCellContent must be (defined) otherwise (
          Warning("Fehlender Befund") at root/Path.Node[TumorCellContent].name
        ) map (_.get) andThen (
          TumorCellContentValidator(TumorCellContent.Method.Bioinformatic)
        ) at report/"Ergebnisse",
        report.results.tmb must be (defined) otherwise (
          Warning("Fehlender Befund") at root/Path.Node[TMB].name
        ) map (_.get) andThen (
          validate(_)
        ) at report/"Ergebnisse",
        report.results.brcaness must be (defined) otherwise (
          Warning("Fehlender Befund") at root/Path.Node[BRCAness].name
        ) map (_.get) andThen (
          validate(_)
        ) at report/"Ergebnisse",
        report.results.hrdScore must be (defined) otherwise (
          Warning("Fehlender Befund") at root/Path.Node[HRDScore].name
        ) map (_.get) andThen (
          validate(_)
        ) at report/"Ergebnisse",
        report.results.simpleVariants must be (nonEmpty) otherwise (
          Warning("Fehlende Befunde") at report/"Ergebnisse"/"Einfache Varianten"
        ) andThen(
          validateEach(_) at report/"Ergebnisse"
        ),
        report.results.copyNumberVariants must be (nonEmpty) otherwise (
          Warning("Fehlende Befunde") at report/"Ergebnisse"/"CNVs"
        ) andThen {
          validateEach(_) at report/"Ergebnisse"
        },
        report.results.dnaFusions must be (nonEmpty) otherwise (
          Info("Fehlende Befunde") at report/"Ergebnisse"/"DNA-Fusionen"
        ) andThen {
          validateEach(_) at report/"Ergebnisse"
        },
        report.results.rnaFusions must be (nonEmpty) otherwise (
          Info("Fehlende Befunde") at report/"Ergebnisse"/"RNA-Fusionen"
        ) andThen {
          validateEach(_) at report/"Ergebnisse"
        }

      )
      .errorsOr(report)


  implicit def medicationRecommendationValidator(
    implicit
    patient: Patient,
    diagnoses: Iterable[MTBDiagnosis],
    variants: Iterable[Variant],
    atc: CodeSystemProvider[ATC,Id,Applicative[Id]],
  ): Validator[Issue,MTBMedicationRecommendation] =
    rec =>
      (
        validate(rec.patient) at rec/"Patient",
        validate(rec.indication) at rec/"Indikation",
        rec.levelOfEvidence must be (defined) otherwise (Warning("Fehlende Angabe")) at rec/"Evidenz-Level",
        rec.medication must be (nonEmpty) otherwise (Error("Fehlende Angabe")) at rec/"Medikation",
        rec.supportingEvidence must be (nonEmpty) otherwise (Warning("Fehlende Angabe")) andThen (
          validateEach(_)
        ) at rec/"Stützende molekulare Alteration",
      )
      .errorsOr(rec)


  implicit def carePlanValidator(
    implicit
    patient: Patient,
    diagnoses: Iterable[MTBDiagnosis],
    variants: Iterable[Variant],
    atc: CodeSystemProvider[ATC,Id,Applicative[Id]],
  ): Validator[Issue,MTBCarePlan] =
    carePlan =>
      (
        validate(carePlan.patient) at carePlan/"Patient",
        validate(carePlan.indication) at carePlan/"Indikation",
        carePlan.medicationRecommendations.filter(_.nonEmpty) must be (defined) orElse (
          carePlan.statusReason must be (defined)
        ) otherwise (
          Error(s"Fehlende Angabe: Es müssen entwder Therapie-Empfehlungen oder Status-Grund '${MTBCarePlan.StatusReason.display(MTBCarePlan.StatusReason.NoTarget)}' aufgeführt sein")
            at carePlan/"Status-Grund"
        ) map (
          _ => carePlan.medicationRecommendations.getOrElse(List.empty)
        ) andThen (validateEach(_) at carePlan/"Therapie-Empfehlungen")
      )
      .errorsOr(carePlan)


  def patientRecordValidator(
    implicit 
    icd10gm: CodeSystemProvider[ICD10GM,Id,Applicative[Id]],
    icdo3: ICDO3.Catalogs[Id,Applicative[Id]],
    atc: CodeSystemProvider[ATC,Id,Applicative[Id]],
    hgnc: CodeSystemProvider[HGNC,Id,Applicative[Id]]
  ): Validator[Issue,MTBPatientRecord] = {
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


      (
        diagnoses must be (nonEmpty) otherwise (
          Error(s"Fehlende Angabe") at root/"Diagnosen"
        ) andThen (
          validateEach(_)
        ),        
        record.getGuidelineMedicationTherapies must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at root/"Leitlinien-Therapien"
        ) andThen {
          implicit val v = GuidelineTherapyValidator
          validateEach(_)
        },
        record.getGuidelineProcedures must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at root/"Leitlinien-Prozeduren"
        ) andThen (
          validateEach(_)
        ),
        record.getPerformanceStatus must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at root/"Performance-Status"
        ) andThen (
          validateEach(_)
        ),
        specimens must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at root/"Tumor-Proben"
        ) andThen (
          validateEach(_)
        ),
        record.getHistologyReports must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at root/"Histologie-Berichte"
        ) andThen (
          validateEach(_)
        ),
        //TODO: IHC-Reports
        record.getNgsReports must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at root/"NGS-Berichte"
        ) andThen (
          validateEach(_)
        ),
        record.getCarePlans must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at root/"MTB-Beschlüsse"
        ) andThen (
          validateEach(_)
        ),
        record.getMedicationTherapies must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at root/"MTB-Therapien"
        ) map (_.flatMap(_.history)) andThen {
          implicit val v = MTBTherapyValidator
          validateEach(_)
        }
        
      )
      .errorsOr(record)
  }

}
