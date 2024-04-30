package de.dnpm.dip.mtb.validation.api



import de.dnpm.dip.service.auth._
import de.dnpm.dip.service.validation.{
  ValidationPermissions,
  ValidationRoles
}



object MTBValidationPermissions extends ValidationPermissions("MTB")


class MTBValidationPermissionsSPI extends PermissionsSPI
{
  override def getInstance: Permissions =
    MTBValidationPermissions
}



object MTBValidationRoles extends ValidationRoles("MTB",MTBValidationPermissions)

class MTBValidationRolesSPI extends RolesSPI
{
  override def getInstance: Roles =
    MTBValidationRoles
}

