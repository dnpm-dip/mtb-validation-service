package de.dnpm.dip.mtb.validation.impl



import de.dnpm.dip.coding.Coding
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.icd.ICD10GM
import de.dnpm.dip.coding.icd.ICDO3
import de.dnpm.dip.model.{
  Id,
  Patient,
  Reference
}
import de.dnpm.dip.mtb.model._


trait Invalidators
{

  def invalidate(diagnosis: MTBDiagnosis) =
    diagnosis.copy(
      patient = Reference.from(Id[Patient]("123")),
      code = Coding[ICD10GM]("wrong"),
      topography = diagnosis.topography.map(_ => Coding[ICDO3.T]("wrong")),
      guidelineTreatmentStatus = None,
    )


  def invalidate(therapy: MTBMedicationTherapy) =
    therapy.copy(
      patient = Reference.from(Id[Patient]("123")),
      therapyLine = None,
      period = None
    )


  def invalidate(procedure: OncoProcedure) =
    procedure.copy(
      patient = Reference.from(Id[Patient]("123")),
      therapyLine = None,
      period = None
    )

  def invalidate(specimen: TumorSpecimen) =
    specimen.copy(
      patient = Reference.from(Id[Patient]("123")),
      `type` = Coding(TumorSpecimen.Type.Unknown)
    )



  def invalidate(record: MTBPatientRecord): MTBPatientRecord =
    record.copy(
      diagnoses =
        Some(record.getDiagnoses.map(invalidate)),
      guidelineMedicationTherapies =
        Some(record.getGuidelineMedicationTherapies.map(invalidate)),
      guidelineProcedures =  
        Some(record.getGuidelineProcedures.map(invalidate)),
      specimens =  
        Some(record.getSpecimens.map(invalidate)),
    )

}
