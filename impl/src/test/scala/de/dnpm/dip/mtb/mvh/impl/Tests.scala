package de.dnpm.dip.mtb.mvh.impl


import org.scalatest.flatspec.AnyFlatSpec
import de.dnpm.dip.mtb.mvh.api.MTBMVHService


class Tests extends AnyFlatSpec
{

  lazy val serviceLoad =
    MTBMVHService.getInstance

  "Loading MTBMVHService" must "have worked" in {
    assert(serviceLoad.isSuccess)
  }

}
