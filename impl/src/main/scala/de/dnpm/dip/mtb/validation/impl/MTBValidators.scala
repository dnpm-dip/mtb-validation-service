package de.dnpm.dip.mtb.validation.impl


import scala.util.chaining._
import cats.{
  Applicative,
  Id
}
import cats.syntax.validated._
import de.ekut.tbi.validation.{
  Validator,
  NegatableValidator
}
import de.ekut.tbi.validation.dsl._
import de.dnpm.dip.util.DisplayLabel
import de.dnpm.dip.coding.{
  Coding,
  CodeSystemProvider
}
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.icd.ICD10GM
import de.dnpm.dip.coding.icd.ICDO3
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.model.{
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
        (
          specimen.`type` match {
            case t @ TumorSpecimen.Type(TumorSpecimen.Type.Unknown) =>
              Warning(s"Fehlende/Unspezifische Angabe '${DisplayLabel.of(t.code).value}'").invalidNel[Coding[TumorSpecimen.Type.Value]] at path/"Typ"
            case t =>
              t.validNel[Issue]
          }
        )
      )
      .errorsOr(specimen)
  }




  def patientRecordValidator(
    implicit 
    icd10gm: CodeSystemProvider[ICD10GM,Id,Applicative[Id]],
    icdo3: ICDO3.Catalogs[Id,Applicative[Id]],
    atc: CodeSystemProvider[ATC,Id,Applicative[Id]]
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
/*      
        record.getPerformanceStatus must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at path/"Performance-Status"
        ) andThen (
          validateEach(_)
        ),
*/      
        record.getSpecimens must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at path/"Tumor-Proben"
        ) andThen (
          validateEach(_)
        ),
        record.getMedicationTherapies must be (nonEmpty) otherwise (
          Warning(s"Fehlende Angabe") at path/"MTB-Therapien"
        ) map (_.flatMap(_.history)) andThen (
          all (_) must be (validMTBTherapy)
        ),
      )
      .errorsOr(record)
  }

}


