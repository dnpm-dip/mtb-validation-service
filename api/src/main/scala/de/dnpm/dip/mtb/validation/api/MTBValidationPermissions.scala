package de.dnpm.dip.mtb.validation.api


import de.dnpm.dip.service.auth._
import de.dnpm.dip.service.validation.ValidationPermissions



object MTBValidationPermissions extends ValidationPermissions("MTB")


class MTBValidationPermissionsSPI extends PermissionsSPI
{
  override def getInstance: Permissions =
    MTBValidationPermissions
}



object MTBValidationRoles extends Roles
{

  val BasicMTBMember =
    Role(
      "MTB-Documentarist",
      MTBValidationPermissions.permissions
    )

  override val roles: Set[Role] =
    Set(BasicMTBMember)

}


class MTBValidationRolesSPI extends RolesSPI
{
  override def getInstance: Roles =
    MTBValidationRoles
}

