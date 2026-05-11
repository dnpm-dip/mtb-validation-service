package de.dnpm.dip.mtb.mvh.impl


import java.nio.file.Files.createTempDirectory
import scala.concurrent.Future
import de.dnpm.dip.mtb.model.MTBPatientRecord
import de.dnpm.dip.service.mvh.FSBackedRepository


final class FakeRepositoryProvider extends RepositoryImplProvider
{
  override def getInstance = {

    val dataDir = createTempDirectory("fs_backed_repo_test_dir").toFile
    dataDir.deleteOnExit

    new FSBackedRepository[Future,MTBPatientRecord](dataDir) with RepositoryImpl
  }

}
