package de.dnpm.dip.mtb.validation.impl



import de.dnpm.dip.coding.{
  Code,
  Coding
}
import de.dnpm.dip.coding.hgvs.HGVS
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

  def invalidate(patient: Patient): Patient =
    patient.copy(
      dateOfDeath = None,
      healthInsurance = None
    )


  def invalidate(diagnosis: MTBDiagnosis): MTBDiagnosis =
    diagnosis.copy(
      patient = Reference.from(Id[Patient]("123")),
      code = Coding[ICD10GM]("wrong"),
      topography = diagnosis.topography.map(_ => Coding[ICDO3.T]("wrong")),
      guidelineTreatmentStatus = None,
    )


  def invalidate(therapy: MTBMedicationTherapy): MTBMedicationTherapy =
    therapy.copy(
      patient = Reference.from(Id[Patient]("123")),
      therapyLine = None,
      period = None
    )


  def invalidate(procedure: OncoProcedure): OncoProcedure =
    procedure.copy(
      patient = Reference.from(Id[Patient]("123")),
      therapyLine = None,
      period = None
    )


  def invalidate(specimen: TumorSpecimen): TumorSpecimen =
    specimen.copy(
      patient = Reference.from(Id[Patient]("123")),
      `type` = Coding(TumorSpecimen.Type.Unknown)
    )


  def invalidate(ngs: NGSReport): NGSReport = {

    def invalidate(snv: SNV): SNV =
      snv.copy(
        proteinChange = snv.proteinChange.map(c => c.copy(code = Code[HGVS]("G12C")))
      )

    ngs.copy(
      results = ngs.results.copy(
        simpleVariants = ngs.results.simpleVariants.map(invalidate)
      )
    )

  }


  def invalidate(record: MTBPatientRecord): MTBPatientRecord =
    record.copy(
      patient =
        invalidate(record.patient),
      diagnoses =
        Some(record.getDiagnoses.map(invalidate)),
      guidelineMedicationTherapies =
        Some(record.getGuidelineMedicationTherapies.map(invalidate)),
      guidelineProcedures =  
        Some(record.getGuidelineProcedures.map(invalidate)),
      specimens =  
        Some(record.getSpecimens.map(invalidate)),
      ngsReports =
        Some(record.getNgsReports.map(invalidate))
    )

}
