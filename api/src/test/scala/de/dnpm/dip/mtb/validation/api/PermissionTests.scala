package de.dnpm.dip.mtb.validation.api


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.Inspectors._
import de.dnpm.dip.service.auth.Permissions


class PermissionTests extends AnyFlatSpec
{

  val spiTry = 
    Permissions.getInstance


  "PermissionSPI" must "have worked" in {

    spiTry.isSuccess mustBe true
   
  }


  "Permission set" must "be non-empty" in {

    Permissions.getAll must not be (empty)

  }


  "Pattern matching of permission names" must "have been successful" in {

    MTBValidationPermissions
      .permissions
      .map(_.name)
      .collect { case MTBValidationPermissions(p) => p }
      .toSet must equal (MTBValidationPermissions.values.toSet)

  }

}
