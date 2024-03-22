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

  implicit val patientNode: Path.Node[Patient] =
    Path.Node("Patient")

  implicit val diagnosisNode: Path.Node[MTBDiagnosis] =
    Path.Node("Diagnose")

  implicit val medTherapyNode: Path.Node[MTBMedicationTherapy] =
    Path.Node("Systemische-Therapie")

  implicit val medTherapyRecommendationNode: Path.Node[MTBMedicationRecommendation] =
    Path.Node("Therapie-Empfehlung")

  private implicit val whoGradingCsp: CodeSystemProvider[WHOGrading,Id,Applicative[Id]] =
    new WHOGrading.Provider.Facade[Id]


  implicit def icdo3TCodingValidator(
    implicit icdo3: ICDO3.Catalogs[Id,Applicative[Id]]
  ): Validator[Issue.Builder,Coding[ICDO3.T]] = {
    coding =>

      implicit val icdo3t =
        coding.version
          .flatMap(icdo3.topography(_))
          .getOrElse(icdo3.topography)

      validate(coding)
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
  ): Validator[Issue,MTBMedicationTherapy] = {
    therapy =>
      val path = basePath / therapy
      (
        validate(therapy.patient) at path/"Patient",
        validate(therapy.indication) at path/"Indikation",
        therapy.therapyLine must be (defined) otherwise (
          Warning("Fehlende Angabe") at path/"Therapie-Linie"
        ),
        therapy.period must be (defined) otherwise (
          Warning("Fehlende Angabe") at path/"Zeitraum"
        ),
        validateOpt(therapy.basedOn) at path/"Empfehlung",
        ifDefined(therapy.medication.map(_.toList))(validateEach(_) at path/"Medikation")
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
  ): Validator[Issue,MTBMedicationTherapy] = {
    therapy =>

      import Therapy.Status.{Ongoing,Completed,Stopped}

      val path = basePath / therapy
      (
        validate(therapy.patient) at path/"Patient",
        validate(therapy.indication) at path/"Indikation",
        validateOpt(therapy.basedOn) at path/"Empfehlung",
        therapy.statusValue match {
          case Ongoing | Completed | Stopped  =>
            therapy.medication.getOrElse(Set.empty).toList must be (nonEmpty) otherwise (
              Error("Fehlende Angabe bei begonnener Therapie") at path/"Medikation"
            ) andThen (
              validateEach(_) at path/"Medikation"
            )
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
          Error(s"Fehlende Angabe") at path/"Leitlinien-Therapien"
        ) andThen (
          all (_) must validGuidelineTherapy
        )
      )
      .errorsOr(record)
  }

}




/*
  implicit def diagnosisValidator(
    implicit
    basePath: Path,
    patient: Patient,
    icd10gm: CodeSystemProvider[ICD10GM,Id,Applicative[Id]]
  ): Validator[Issue,MTBDiagnosis] = {
    diagnosis =>
      val path = basePath / diagnosis
      (
        validate(diagnosis.patient) otherwise (
          Error("Ungültige Referenz auf 'Patient'") at path/"Patient"
        ),
        validate(diagnosis.code) otherwise (
          Error("Ungültiger ICD-10-GM Code oder Version") at path/"Code"
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
  ): Validator[Issue,MTBMedicationTherapy] = {
    therapy =>
      val path = basePath / therapy
      (
        validate(therapy.patient) otherwise (
          Error("Ungültige Referenz auf 'Patient'") at path/"Patient"
        ),
        validate(therapy.indication) otherwise (
          Error("Ungültige Referenz auf 'Diagnose'") at path/"Indikation"
        ),
        therapy.therapyLine must be (defined) otherwise (
          Warning("Fehlende Angabe") at path/"Therapie-Linie"
        ),
        therapy.period must be (defined) otherwise (
          Warning("Fehlende Angabe") at path/"Zeitraum"
        ),
        validateEach(therapy.basedOn) otherwise (
          Error("Ungültige Referenz auf 'Therapie-Empfehlung'") at path/"Empfehlung"
        ),
        validateEach(therapy.medication.getOrElse(Set.empty).toList) otherwise (
          Error("Ungültiger ATC Code oder Version") at path/"Medikation"
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
  ): Validator[Issue,MTBMedicationTherapy] = {
    therapy =>

      import Therapy.Status.{Ongoing,Completed,Stopped}

      val path = basePath / therapy
      (
        validate(therapy.patient) otherwise (
          Error("Ungültige Referenz auf 'Patient'") at path/"Patient"
        ),
        validate(therapy.indication) otherwise (
          Error("Ungültige Referenz auf 'Diagnose'") at path/"Indikation"
        ),
        validateEach(therapy.basedOn) otherwise (
          Error("Ungültige Referenz auf 'Therapie-Empfehlung'") at path/"Empfehlung"
        ),
        therapy.statusValue match {
          case Ongoing | Completed | Stopped  =>
            therapy.medication must be (defined) otherwise (
              Error("Fehlende Angabe bei begonnener Therapie") at path/"Medikation"
            ) andThen (
              medications => validateEach(medications.get.toList) otherwise (
                Error("Ungültiger ATC Code oder Version") at path/"Medikation"
              )
            )
          case _ =>
            None.validNel[Issue]
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
              _.get.endOption must be (defined) otherwise (Error("Fehlende Angabe") at path/"Zeitraum"/"End-Datum")
            )
            
          case _ =>
            None.validNel[Issue]
        },
        therapy.statusReason must be (defined) otherwise (
          Warning("Fehlende Angabe") at path/"Status-Grund"
        )
      )
      .errorsOr(therapy)
  }
*/


