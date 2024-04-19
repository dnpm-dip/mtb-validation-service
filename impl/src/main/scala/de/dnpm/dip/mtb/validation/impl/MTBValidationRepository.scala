package de.dnpm.dip.mtb.validation.impl


import java.io.File
import scala.concurrent.Future
import scala.util.chaining._
import cats.Monad
import de.dnpm.dip.util.Logging
import de.dnpm.dip.service.validation.{
  Repository,
  FSBackedRepository,
  InMemRepository
}
import de.dnpm.dip.mtb.model.MTBPatientRecord



object MTBValidationRepository extends Logging
{

  private[impl] val dataDirProp =
    "dnpm.dip.data.dir"


  lazy val instance: Repository[Future,Monad[Future],MTBPatientRecord] = 
    Option(System.getProperty(dataDirProp)).map(dir => s"$dir/mtb_data/validation") match {

      case Some(dir) =>
        val dataDir = new File(dir)
        dataDir.mkdirs
        new FSBackedRepository[Future,MTBPatientRecord](dataDir)

      case None =>
        log.warn(s"System property $dataDirProp for the data storage directory is undefined. Falling back to in-memory (i.e. non-persisting) repository implementation)")
        new InMemRepository[Future,MTBPatientRecord]
/*        
        val msg =
          s"System property $dataDirProp for the data storage directory is undefined!"
           .tap(log.error)
        throw new IllegalStateException(msg)
*/        
    }

}
