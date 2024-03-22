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
      guidelineTreatmentStatus = None,
      topography = None
    )

}
