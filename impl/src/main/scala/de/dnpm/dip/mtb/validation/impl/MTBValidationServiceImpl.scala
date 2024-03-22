package de.dnpm.dip.mtb.validation.impl


import scala.concurrent.Future
import cats.Monad
import de.ekut.tbi.validation.Validator
import de.dnpm.dip.service.validation.{
  BaseValidationService,
  Issue,
  Repository
}
import de.dnpm.dip.mtb.model.MTBPatientRecord
import de.dnpm.dip.mtb.validation.api.{
  MTBValidationService,
  MTBValidationServiceProvider
}


class MTBValidationServiceImpl
(
  private val validator: Validator[Issue,MTBPatientRecord],
  private val repo: Repository[Future,Monad[Future],MTBPatientRecord]
)
extends BaseValidationService(
  validator,
  Issue.Severity.Error,
  repo
)
with MTBValidationService
