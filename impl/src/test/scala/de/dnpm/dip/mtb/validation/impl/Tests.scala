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

    val tCodes =
      List(
       "TX",
       "T0",
       "Tis",
       "T1",
       "T1a",
       "T1(m)",
       "T1(3)",
       "T1a2",
       "T1a(2)",
       "T1a1",
       "T1a2",
       "T1b",
       "T1b1",
       "T1b2",
       "T1b3",
       "T1c",
       "T1c1",
       "T1c2",
       "T1c3",
       "T1d",
       "T1mi",
       "T2",
       "T2a",
       "T2a1",
       "T2a2",
       "T2b",
       "T3",
       "T4"
      )

    val nCodes = List("N1b","pN1(mi)","pN0(i-)")
    val mCodes = List("pM1","cM0")

    all (tCodes) must fullyMatch regex TNM.tGroupPattern
    all (nCodes) must fullyMatch regex TNM.nGroupPattern
    all (mCodes) must fullyMatch regex TNM.mGroupPattern

  }

}
