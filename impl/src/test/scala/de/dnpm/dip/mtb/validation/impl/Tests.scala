package de.dnpm.dip.mtb.validation.impl


import scala.util.Random
import scala.util.chaining._
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.must.Matchers._
import de.ekut.tbi.generators.Gen
import de.dnpm.dip.mtb.model.MTBPatientRecord
import de.dnpm.dip.service.DataUpload
import de.dnpm.dip.mtb.validation.api.MTBValidationService
import de.dnpm.dip.service.validation.ValidationService.{
  Validate,
  FatalIssuesDetected
}
import de.dnpm.dip.mtb.gens.Generators._
import play.api.libs.json.Json.{ 
  toJson,
  prettyPrint
}


class Tests extends AsyncFlatSpec with Invalidators
{

  implicit val rnd: Random =
    new Random

  val record =
    Gen.of[MTBPatientRecord].next
      .pipe(invalidate)

  lazy val serviceLoad =
    MTBValidationService.getInstance

  lazy val service =
    serviceLoad.get


  "Loading MTBValidationService" must "have worked" in {
    assert(serviceLoad.isSuccess)
  }


  "Validation of invalidated MTBPatientRecord" must "have failed" in {

    (service ! Validate(DataUpload(record,None))).map {

      case Left(FatalIssuesDetected(report)) =>
        toJson(report) pipe prettyPrint pipe println
        succeed

      case _ => fail()
    }

  }


  "TNM code validations" must "have behaved correctly" in { 

    val tCodes = List("T1(m)","T1(3)")
    val nCodes = List("N1b","pN1(mi)","pN0(i-)")
    val mCodes = List("pM1")

    all (tCodes) must fullyMatch regex TNM.tGroupPattern
    all (nCodes) must fullyMatch regex TNM.nGroupPattern
    all (mCodes) must fullyMatch regex TNM.mGroupPattern

  }

}
