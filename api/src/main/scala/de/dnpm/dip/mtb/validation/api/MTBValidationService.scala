package de.dnpm.dip.mtb.validation.api


import scala.concurrent.Future
import cats.Monad
import de.dnpm.dip.util.{
  SPI,
  SPILoader
}
import de.dnpm.dip.service.validation.ValidationService
import de.dnpm.dip.mtb.model.MTBPatientRecord



trait MTBValidationService extends ValidationService[
  Future,
  Monad[Future],
  MTBPatientRecord
]


trait MTBValidationServiceProvider extends SPI[MTBValidationService]

object MTBValidationService extends SPILoader[MTBValidationServiceProvider]

