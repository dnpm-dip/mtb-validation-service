package de.dnpm.dip.mtb.validation.impl


import scala.concurrent.Future
import de.dnpm.dip.mtb.model.MTBPatientRecord
import de.dnpm.dip.service.validation.InMemRepository


class TestRepositoryProvider extends MTBValidationRepositoryProvider
{

  override def getInstance = 
    new InMemRepository[Future,MTBPatientRecord] with MTBValidationRepository

}
