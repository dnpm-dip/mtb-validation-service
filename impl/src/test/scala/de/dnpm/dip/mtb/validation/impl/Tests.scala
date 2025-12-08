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




  private val tCode = "T".r.unanchored
  private val nCode = "N".r.unanchored
  private val mCode = "M".r.unanchored

  // Values from some Excel sheet found from BfArM
  private val tnmCodes =
    Seq(
      "pN2b(sn)","pN2a(sn)","pN2(sn)","pN1c(sn)","pN1b(sn)","pNX(sn)","pN1mi(sn)","pN1(sn)",
      "cNX(sn)","cN1a(sn)","cN1(sn)","cN0(sn)",
      "ycN3b","ycN3a","ycN3","ycN2mi","ycN0(i+)","ycN2a","ycN1a","ycN1c","ycN2b","ycN2c","ycN2","ycN1mi","ycN1","ycN1b","ycNX","ycN3c","ycN0",
      "cN3a","cN2b","cN3b","cN3","cN2mi","cN3c","cN2a","cN2c","cN1b","cN2","cN1a","cN1mi","cN1c","cN1","cN0(i+)","cN0","cNX",
      "rcT1c1","rcT2c","rcT2a2","rcT1b2","rcT4d","rcT1b","rcT3b","rcT1a1","rcT3d","rcT3","rcT1mi","rcT1c3","rcT3a","rcTis","rcTis(Paget)",
      "rcTis(DCIS)","rcT2a1","rcT4a","rcT2","rcT2b","rcTa","rcT0","rcT1c","rcT1b1","rcT4e","rcT1a2","rcT2d","rcT4c","rcT1c2","rcT3c","rcT1d","rcT4","rcT1a",
      "rcT2a","rcT4b","rcTis(LAMN)","rcT1","rcT2mi","rcTX",
      "cT4","cT1","cT1c1","cT3d","cT2","cT1b2","cT3b","cT2a1","cT3a","cT1a2","cTis(Paget)","cT0","cT1b1","cTis","cT1a","cT4a","cT4e","cT4c","cT1mi","cT1d","cT1c2","cT3c",
      "cTX","cT3e","cT1c","cT2b","cT2d","cT1b3","cT2c","cT1b","cT2a2","cT3","cTis(DCIS)","cTis(LAMN)","cT4d","cT2a","cTa","cT4b","cT1a1","cT1c3",
      "ypNX","ypN1c","ypN2b","ypN2c","ypN2","ypN3b","ypN3","ypN2a","ypN3a","ypN1b","ypN3c","ypN1a(sn)","ypN0(mol+)","ypN2mi","ypN0(i+)","ypN0",
      "ypN1mi","ypN1a","ypN1","ycT3e","ycTa","ycTis(Paget)","ycTis(DCIS)","ycTX","ycT4","ycT0","ycT1d","ycT1b3","ycT1c","ycT4d","ycT4b","ycT1c2","ycT3","ycT1b1","ycT1a","ycT2c",
      "ycT2a2","ycT1a2","ycTis","ycTis(LAMN)","ycT1","ycT3d","ycT3b","ycT2a","ycT2b","ycT2","ycT1c3","ycT1a1","ycT1mi","ycT1b2","ycT3a","ycT2a1","ycT1c1","ycT3c","ycT2d","ycT4e",
      "ycT1b","ycT4a","ycT4c",
      "rcN1","rcNX","rcN3c","rcN3","rcN2c","rcN2b","rcN3a","rcN3b","rcN2a","rcN1a","rcN2","rcN1b","rcN0(i+)","rcN1mi","rcN0","rcN1c",
      "rpM1d(1)","rpM1a(0)","rpM1d","rpM1","rpM1c(1)","rpM1b(0)","rpM1c","rpM1a(1)","rpM1d(0)","rpM1a",
      "rcM1d(0)","rcM1a(0)","rcM1b","rcM1d","rcM1c(0)","rcM1b(0)","rcM1a(1)","rcM1a","rcM0(i+)",
      "rcM1c(1)","rcM1c","rcM1d(1)","rcM1b(1)","rcM1","rcM0",
      "rpM1c(0)","rpM1b","rpM1b(1)","rpT1d","rpTis(LAMN)","rpT2a2","rpT2a","rpT0","rpT1","rpT4e","rpT1c1","rpT1b2","rpT1a2","rpT1c2","rpT1c","rpT4d","rpT4","rpT3c","rpTX","rpT1b",
      "rpT4b","rpT3a","rpT1a1","rpTis(Paget)","rpT2d","rpT2b","rpT1mi","rpTis(DCIS)","rpT2","rpT1c3","rpT2a1","rpTa","rpT4a","rpT3b","rpT1b1","rpT4c","rpT3d","rpT1a","rpT3","rpT2c","rpTis",
      "ypT3d","ypT1b2","ypT1c1","ypT4e","ypT1c3","ypT2a","ypT1a1","ypT3","ypT2a2","ypT2b","ypT3a","ypTis","ypT2a1","ypT2d","ypT0","ypT4","ypT4b","ypT4d","ypT3c","ypT1c2","ypT1a",
      "ypT1b1","ypT1b3","ypT1c","ypT1","ypT1b","ypTis(LAMN)","ypT1mi","ypT1a2","ypT2","ypT1d","ypTis(DCIS)","ypTa","ypTX","ypT3b","ypT2c","ypTis(Paget)","ypT4c","ypT4a",
      "pN0(i+)","pN3b","pN3","pN0","pN1mi","pN1a","pN3a","pN1c","pN1b","pN1a(sn)","pN1","pN2c","pN2b","pN2mi","pN2","pN0(mol+)","pN2a","pN3c","pNX",
      "rpN1c","rpN1a","rpNX","rpN0(i+)","rpN1mi","rpN1b","rpN2c","rpN2mi","rpN3c","rpN3","rpN2b","rpN3a","rpN3b","rpN2a","rpN0a","rpN1","rpN2","rpN0(mol+)","rpN0",
      "pM1","pM1b(1)","pM1c(1)","pM1d","pM1d(1)","pM1b","pM1d(0)","pM1a(0)","pM1c(0)","pM1c","pM1a","pM1a(1)","pM1b(0)",
      "pT2c","pT1d","pT2a2","pT1a2","pT1b3","pT2a","pTis(Paget)","pT1mi","pT4b","pT1a","pT1","pTis","pT4d","pT3c","pTis(LAMN)","pT1b2","pT4","pT0","pT3a","pT1a1","pT1c3","pT2d",
      "pT2b","pT2a1","pT2","pT1c1","pT1b","pT4e","pT1c2","pT4c","pTa","pT4a","pT1c","pT1b1","pTis(DCIS)","pT3b","pT3","pTX","pT3d",
      "cM1c(1)","cM1c(0)","cM1b(0)","cM1d(1)","cM1d(0)","cM1a(0)","cM1a(1)","cM1a","cM1c","cM1b(1)","cM1b","cM1","cM0","cM0(i+)","cM1d",
      "rcM0(mol+)","cM0(mol+)",
      "ycTis(LCIS)","rpTis(LCIS)","pTis(LCIS)","cTis(LCIS)","ypTis(LCIS)"
    )
    .groupBy(
      code => code match {
        case tCode() => "T"
        case nCode() => "N"
        case mCode() => "M"
      }
    )
 
 
 "TNM code validations" must "have behaved correctly" in { 

    all (tnmCodes("T")) must fullyMatch regex TNM.tGroupPattern
    all (tnmCodes("N")) must fullyMatch regex TNM.nGroupPattern
    all (tnmCodes("M")) must fullyMatch regex TNM.mGroupPattern

  }

}
