package de.dnpm.dip.mtb.validation.impl


import scala.util.Random
import scala.util.chaining._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.Inspectors._
import cats.{
  Applicative,
  Id
}
import de.ekut.tbi.generators.Gen
import de.ekut.tbi.validation.dsl._
import de.dnpm.dip.coding.{
  Coding,
  CodeSystemProvider
}
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.icd.ICD10GM
import de.dnpm.dip.coding.icd.ICDO3
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.model.Patient
import de.dnpm.dip.mtb.model._
import de.dnpm.dip.mtb.gens.Generators._
import de.dnpm.dip.service.validation.Issue.Path
import MTBValidators._
import play.api.libs.json.Json.{ 
  toJson,
  prettyPrint
}



class ValidatorTests extends AnyFlatSpec
with Invalidators
{

  implicit val rnd: Random =
    new Random

  implicit val hgnc: CodeSystemProvider[HGNC,Id,Applicative[Id]] =
    HGNC.GeneSet
      .getInstance[Id]
      .get

  implicit val atc: CodeSystemProvider[ATC,Id,Applicative[Id]] =
    ATC.Catalogs
      .getInstance[Id]
      .get


  implicit val icd10gm: CodeSystemProvider[ICD10GM,Id,Applicative[Id]] =
    ICD10GM.Catalogs
      .getInstance[Id]
      .get

  implicit val icdo3: ICDO3.Catalogs[Id,Applicative[Id]] =
    ICDO3.Catalogs
      .getInstance[Id]
      .get


  implicit val path: Path =
    Path.root

  val record =
    Gen.of[MTBPatientRecord].next

  implicit val patient: Patient =
    record.patient


  "Validation of MTBDiagnosis" must "have failed" in {
 
    val diagnoses =
      record.getDiagnoses
        .map(invalidate)

    forAll(
      diagnoses.map(validate(_))
        .tapEach(
          _.fold(
            errs => toJson(errs.toList) pipe prettyPrint pipe println,
            _ => ()
          )
        )
    ){ 
      _.isInvalid mustBe true
    }

  }


}
