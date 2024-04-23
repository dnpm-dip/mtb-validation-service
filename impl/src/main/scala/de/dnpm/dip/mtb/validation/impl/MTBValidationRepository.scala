package de.dnpm.dip.mtb.validation.impl


import java.io.File
import scala.concurrent.Future
import scala.util.chaining._
import cats.Monad
import de.dnpm.dip.util.{
  SPI,
  SPILoader
}
import de.dnpm.dip.service.validation.{
  Repository,
  FSBackedRepository
}
import de.dnpm.dip.mtb.model.MTBPatientRecord


trait MTBValidationRepository extends Repository[Future,Monad[Future],MTBPatientRecord]

trait MTBValidationRepositoryProvider extends SPI[MTBValidationRepository]

object MTBValidationRepository extends SPILoader[MTBValidationRepositoryProvider]
{

  private[impl] val dataDirProp =
    "dnpm.dip.data.dir"


  override def getInstance =
    super.getInstance // Load implementation from runtime context (e.g. test implementation)...
      .recover {      // ... else default to file system-backed repo
        case t =>
          Option(System.getProperty(dataDirProp)) match {
            case Some(dir) =>
              val validationDir = new File(s"$dir/mtb_data/validation")
              validationDir.mkdirs
              new FSBackedRepository[Future,MTBPatientRecord](validationDir)
                with MTBValidationRepository
          
            case None =>
              val msg =
                s"System property $dataDirProp for the data storage directory is undefined, can't instantiate validation data repository!"
                 .tap(log.error)
              throw new IllegalStateException(msg)
          }
      }

}
