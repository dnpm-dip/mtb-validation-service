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
import de.dnpm.dip.coding.{
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



object MTBValidators extends Validators
{


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

  implicit val snvNode: Path.Node[SNV] =
    Path.Node("SNV")

  implicit val cnvNode: Path.Node[CNV] =
    Path.Node("CNV")



  // For implicit conversions to NegatableValidator[Issue,T]
  private implicit def defaultIssueAtPath(
    implicit path: Path
  ): String => Issue =
    Error(_) at path


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
    basePath: Path,
    patient: Patient,
    icd10gm: CodeSystemProvider[ICD10GM,Id,Applicative[Id]],
    icdo3: ICDO3.Catalogs[Id,Applicative[Id]],
  ): Validator[Issue,MTBDiagnosis] = {
    diagnosis =>
      val path = basePath / diagnosis
      (
        validate(diagnosis.patient) at path/"Patient",
        validate(diagnosis.code) at path/"Code",
        diagnosis.topography must be (defined) otherwise (
          Info("Fehlende Angabe")
        ) andThen (
          c => validate(c.get)
        ) at path/"Topographie",
        ifDefined(diagnosis.whoGrading)(validate(_) at path/"WHO-Graduierung"),
        diagnosis.guidelineTreatmentStatus must be (defined) otherwise (
          Warning("Fehlende Angabe") at path/"Leitlinien-Behandlungsstatus"
        )
      )
      .errorsOr(diagnosis)
  }


  def validGuidelineTherapy(
    implicit
    basePath: Path,
    patient: Patient,
    diagnoses: Iterable[MTBDiagnosis],
    recommendations: Iterable[MTBMedicationRecommendation],
    atc: CodeSystemProvider[ATC,Id,Applicative[Id]]
  ): NegatableValidator[Issue,MTBMedicationTherapy] = {
    therapy =>
      val path = basePath / therapy
      (
        validate(therapy),
        validateOpt(therapy.basedOn) at path/"Empfehlung",
        therapy.medication.map(_.toList)
          .pipe(
            ifDefined(_)(validateEach(_) at path/"Medikation")
          )
      )
      .errorsOr(therapy)
  }


  def validMTBTherapy(
    implicit
    basePath: Path,
    patient: Patient,
    diagnoses: Iterable[MTBDiagnosis],
    recommendations: Iterable[MTBMedicationRecommendation],
    atc: CodeSystemProvider[ATC,Id,Applicative[Id]]
  ): NegatableValidator[Issue,MTBMedicationTherapy] = {
    therapy =>

      import Therapy.Status.{Ongoing,Completed,Stopped}

      val path = basePath / therapy
      (
        validate(therapy),
        validateOpt(therapy.basedOn) at path/"Empfehlung",
        therapy.statusValue match {
          case Ongoing | Completed | Stopped  =>
            therapy.medication.getOrElse(Set.empty).toList must be (nonEmpty) otherwise (
              Error("Fehlende Angabe bei begonnener Therapie")
            ) andThen (
              validateEach(_)
            ) at path/"Medikation"
          case _ => None.validNel[Issue]
        },
        therapy.statusValue match {
          case Ongoing =>
            therapy.period must be (defined) otherwise (
              Error("Fehlende Angabe bei begonnener Therapie") at path/"Zeitraum"
            )
          case Completed | Stopped =>
            therapy.period must be (defined) otherwise (
              Error("Fehlende Angabe bei begonnener Therapie") at path/"Zeitraum"
            ) andThen (
              _.get.endOption must be (defined) otherwise (
                Error("Fehlende Angabe bei angeschlossener Therapie") at path/"Zeitraum"/"End-Datum"
              )
            )
            
          case _ => None.validNel[Issue]
        },
        therapy.statusReason must be (defined) otherwise (
          Warning("Fehlende Angabe") at path/"Status-Grund"
        )
      )
      .errorsOr(therapy)
  }

  
  implicit def specimenValidator(
    implicit
    basePath: Path,
    patient: Patient,
    diagnoses: Iterable[MTBDiagnosis],
  ): NegatableValidator[Issue,TumorSpecimen] = {
    specimen =>
      val path = basePath / specimen
      (
        validate(specimen.patient) at path/"Patient",
        validate(specimen.diagnosis) at path/"Diagnose",
        specimen.`type` must not (be (Coding(TumorSpecimen.Type.Unknown))) otherwise (
          Warning(s"Fehlende/Unspezifische Angabe '${DisplayLabel.of(specimen.`type`.code).value}'")
        ) at path/"Typ"
      )
      .errorsOr(specimen)
  }


  private val tumorCellContentRange =
    ClosedInterval(0.0 -> 1.0)

  private def TumorCellContentValidator(
    basePath: Path,
    method: TumorCellContent.Method.Value
  )(
    implicit
    patient: Patient,
    specimens: Iterable[TumorSpecimen],
  ): Validator[Issue,TumorCellContent] = {
    tcc =>
      val path = basePath / tcc
      val expectedMethod = Coding(method)
      (
        validate(tcc.patient) at path/"Patient",
        validate(tcc.specimen) at path/"Probe",
        tcc.method must be (expectedMethod) otherwise (
          Error(s"Ungültige Bestimmungs-Methode, '${DisplayLabel.of(expectedMethod)}' erwartet")
        ) at path/"Methode",
        tcc.value must be (in (tumorCellContentRange)) otherwise (
          Error(s"Ungültiger Wert ${tcc.value}, nicht in Referenz-Bereich $tumorCellContentRange")
        ) at path/"Wert"
      )
      .errorsOr(tcc)
  }


  implicit def tumorMorphologyValidator(
    implicit
    basePath: Path,
    patient: Patient,
    specimens: Iterable[TumorSpecimen],
    icdo3: ICDO3.Catalogs[Id,Applicative[Id]]
  ): Validator[Issue,TumorMorphology] = {
    obs =>
      val path = basePath / obs
      (
        validate(obs.patient) at path/"Patient",
        validate(obs.specimen) at path/"Probe",
        validate(obs.value) at path/"Wert"
      )
      .errorsOr(obs)
  }


  implicit def histologyReportValidator(
    implicit
    basePath: Path,
    patient: Patient,
    specimens: Iterable[TumorSpecimen],
    icdo3: ICDO3.Catalogs[Id,Applicative[Id]],
  ): NegatableValidator[Issue,HistologyReport] = {
     report =>

      val path = basePath / report

      val tumorMorphology  = report.results.tumorMorphology
      val tumorCellContent = report.results.tumorCellContent
      val expectedMethod   = Coding(TumorCellContent.Method.Histologic).code

      (
        validate(report.patient) at path/"Patient",
        validate(report.specimen) at path/"Probe",
        (tumorMorphology orElse tumorCellContent) must be (defined) otherwise (
          Error("Keine Befunde vorhanden, weder Tumor-Morphologie noch -Zellgehalt") at path/"Ergebnisse"
        ),
        tumorMorphology must be (defined) otherwise (
          Warning("Fehlender Befund") at path/"Ergebnisse"/Path.Node[TumorMorphology].name
        ) map (_.get) andThen (
          validate(_) 
        ),
        tumorCellContent must be (defined) otherwise (
          Warning("Fehlender Befund") at path/"Ergebnisse"/Path.Node[TumorCellContent].name
        ) map (_.get) andThen (
          TumorCellContentValidator(path/"Ergebnisse",TumorCellContent.Method.Histologic)
        )
      )
      .errorsOr(report)

  }



  private def validSNV(
    path: Path
  )(
    implicit
    patient: Patient,
    geneValidator: Validator[Issue.Builder,Coding[HGNC]]
  ): NegatableValidator[Issue,SNV] = {
    
    implicit val implPath = path

    snv =>
      (
        validate(snv.patient) at path/snv/"Patient",
        ifDefined(snv.gene)(validate(_)) at path/snv/"Gen"
      )
      .errorsOr(snv)
  }

  private def validCNV(
    path: Path
  )(
    implicit
    patient: Patient,
    geneValidator: Validator[Issue.Builder,Coding[HGNC]]
  ): NegatableValidator[Issue,CNV] = {
    
    implicit val implPath = path

    cnv =>
      (
        validate(cnv.patient) at path/cnv/"Patient",
        validateEach(cnv.reportedAffectedGenes.toList) at path/cnv/"Reported Affected Genes",
        validateEach(cnv.copyNumberNeutralLoH.toList) at path/cnv/"CopyNumber Neutral LoH"
      )
      .errorsOr(cnv)
  }

  implicit def ngsReportValidator(
    implicit
    basePath: Path,
    patient: Patient,
    specimens: Iterable[TumorSpecimen],
    hgnc: CodeSystemProvider[HGNC,Id,Applicative[Id]]
  ): NegatableValidator[Issue,NGSReport] = {
    report =>
      val path = basePath/report

      (
        validate(report.patient) at path/"Patient",
        validate(report.specimen) at path/"Probe",
        report.results.tumorCellContent must be (defined) otherwise (
          Warning("Fehlender Befund") at path/"Ergebnisse"/Path.Node[TumorCellContent].name
        ) map (_.get) andThen (
          TumorCellContentValidator(path/"Ergebnisse",TumorCellContent.Method.Bioinformatic)
        ),
        report.results.tmb must be (defined) otherwise (
          Warning("Fehlender Befund") at path/"Ergebnisse"/Path.Node[TMB].name
        ) map (_.get) andThen (
          ObservationValidator[TMB.Result,TMB](
            path/"Ergebnisse",
            tmb => tmb must be (in (TMB.referenceRange)) otherwise (
              Error(s"Ungültiger Wert ${tmb.value}, nicht in Referenz-Bereich ${TMB.referenceRange}")
            )
          )
        ),
        report.results.brcaness must be (defined) otherwise (
          Warning("Fehlender Befund") at path/"Ergebnisse"/Path.Node[BRCAness].name
        ) map (_.get) andThen (
          ObservationValidator[Double,BRCAness](
            path/"Ergebnisse",
            value => value must be (in (BRCAness.referenceRange)) otherwise (
              Error(s"Ungültiger Wert $value, nicht in Referenz-Bereich ${BRCAness.referenceRange}")
            )
          )
        ),
        report.results.hrdScore must be (defined) otherwise (
          Warning("Fehlender Befund") at path/"Ergebnisse"/Path.Node[HRDScore].name
        ) map (_.get) andThen (
          ObservationValidator[Double,HRDScore](
            path/"Ergebnisse",
            value => value must be (in (HRDScore.referenceRange)) otherwise (
              Error(s"Ungültiger Wert $value, nicht in Referenz-Bereich ${HRDScore.referenceRange}")
            )
          )
        ),
        report.results.simpleVariants must be (nonEmpty) otherwise (
          Warning("Fehlende Befunde") at path/"Ergebnisse"/"SNVs"
        ) andThen(
          all(_) must be (validSNV(path/"Ergebnisse"))
        ),
        report.results.copyNumberVariants must be (nonEmpty) otherwise (
          Warning("Fehlende Befunde") at path/"Ergebnisse"/"CNVs"
        ) andThen {
          all(_) must be (validCNV(path/"Ergebnisse"))
        }
        


      )
      .errorsOr(report)

  }






  def patientRecordValidator(
    implicit 
    icd10gm: CodeSystemProvider[ICD10GM,Id,Applicative[Id]],
    icdo3: ICDO3.Catalogs[Id,Applicative[Id]],
    atc: CodeSystemProvider[ATC,Id,Applicative[Id]],
    hgnc: CodeSystemProvider[HGNC,Id,Applicative[Id]]
  ): Validator[Issue,MTBPatientRecord] = {
    record =>

      implicit val path =
        Path.root

      implicit val patient =
        record.patient

      implicit val diagnoses = 
        record.getDiagnoses

      implicit val recommendations =
        record.getCarePlans
          .flatMap(_.medicationRecommendations.getOrElse(List.empty))

      implicit val specimens = 
        record.getSpecimens

      implicit val performanceStatusValidator: Validator[Issue,PerformanceStatus] =
        ObservationValidator[Coding[ECOG.Value],PerformanceStatus](
          path,
          csCodingValidator(ECOG.codeSystem)
        )



      (
        diagnoses must be (nonEmpty) otherwise (
          Error(s"Fehlende Angabe") at path/"Diagnosen"
        ) andThen (
          validateEach(_)
        ),        
        record.getGuidelineMedicationTherapies must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at path/"Leitlinien-Therapien"
        ) andThen (
          all (_) must be (validGuidelineTherapy)
        ),
        record.getGuidelineProcedures must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at path/"Leitlinien-Prozeduren"
        ) andThen (
          validateEach(_)
        ),
        record.getPerformanceStatus must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at path/"Performance-Status"
        ) andThen (
          validateEach(_)
        ),
        specimens must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at path/"Tumor-Proben"
        ) andThen (
          validateEach(_)
        ),
        record.getMedicationTherapies must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at path/"MTB-Therapien"
        ) map (_.flatMap(_.history)) andThen (
          all (_) must be (validMTBTherapy)
        ),
        record.getHistologyReports must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at path/"Histologie-Berichte"
        ) andThen (
          validateEach(_)
        ),
        //TODO: IHC-Reports
        record.getNgsReports must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at path/"NGS-Berichte"
        ) andThen (
          validateEach(_)
        )
        
      )
      .errorsOr(record)
  }

}


