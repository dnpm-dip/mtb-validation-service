package de.dnpm.dip.mtb.mvh.impl


import scala.concurrent.Future
import cats.Monad
import de.dnpm.dip.service.mvh.{
  BaseMVHService,
  BaseReport,
  Report,
  Repository,
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

  override def report(
    criteria: Report.Criteria
  )(
    implicit env: Monad[Future]
  ): Future[BaseReport] =  
    env.map(baseReport(criteria))(_._1)

}
