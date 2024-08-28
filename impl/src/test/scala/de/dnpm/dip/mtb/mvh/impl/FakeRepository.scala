package de.dnpm.dip.mtb.mvh.impl


import scala.concurrent.Future
import de.dnpm.dip.mtb.model.MTBPatientRecord
import de.dnpm.dip.service.mvh.InMemRepository


final class FakeRepositoryProvider extends RepositoryImplProvider
{
  override def getInstance =
    new InMemRepository[Future,MTBPatientRecord] with RepositoryImpl

}
