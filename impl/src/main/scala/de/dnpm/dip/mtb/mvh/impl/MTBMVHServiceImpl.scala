package de.dnpm.dip.mtb.mvh.impl


import scala.util.chaining._
import scala.concurrent.Future
import cats.Monad
import de.dnpm.dip.service.mvh.{
  BaseMVHService,
  BaseReport,
  Report,
  Repository,
  Submission,
  UseCase
}
import de.dnpm.dip.mtb.mvh.api.{
  MTBMVHService,
  MTBMVHServiceProvider
}
import de.dnpm.dip.mtb.model.MTBPatientRecord



class MTBMVHServiceProviderImpl extends MTBMVHServiceProvider
{
  override def getInstance: MTBMVHService =
    MTBMVHServiceImpl.instance
}


object MTBMVHServiceImpl
{
  val instance =
    new MTBMVHServiceImpl(
      RepositoryImpl.getInstance.get
    )
}


class MTBMVHServiceImpl(
  repo: Repository[Future,Monad[Future],MTBPatientRecord]
)
extends BaseMVHService(
  UseCase.MTB,
  repo
)
with MTBMVHService
{

  import de.dnpm.dip.service.mvh.extensions._

  override def sequenceTypes(
    record: MTBPatientRecord
  ): Option[Set[Submission.SequenceType.Value]] = {

    val mvhBoardDate = record.mvhCarePlan.map(_.issuedOn)

    record.ngsReports.map(
      _.filter(report => mvhBoardDate.fold(false)(report.issuedOn isAfter _))  // Keep only NGS-Reports after the MVH-Board-Date
       .foldLeft(Set.empty[Submission.SequenceType.Value])(
         (seqTypes,report) =>
           seqTypes.pipe(
             set =>
               if (report.results.simpleVariants.exists(_.nonEmpty) || report.results.copyNumberVariants.exists(_.nonEmpty) || report.results.dnaFusions.exists(_.nonEmpty))
                 set + Submission.SequenceType.DNA
               else set
           )
           .pipe(
             set => 
               if (report.results.rnaFusions.exists(_.nonEmpty) || report.results.rnaSeqs.exists(_.nonEmpty))
                 set + Submission.SequenceType.RNA
               else set
           )
       )
    )

  } 

  override def report(
    criteria: Report.Criteria
  )(
    implicit env: Monad[Future]
  ): Future[BaseReport] =  
    env.map(baseReport(criteria))(_._1)

}
