#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
.onAttach <- function(libname, pkgname) {
  Mar.utils::updateCheck(gitPkg = 'Maritimes/Mar.fleets')
  localVer = utils::packageDescription('Mar.fleets')$Version
  packageStartupMessage(paste0("Version: ", localVer))
}

.onLoad <- function(libname, pkgname){
  options(stringsAsFactors = FALSE)
  Sys.setenv(TZ = "America/Halifax")
}

utils::globalVariables(c("GEARS","LIC_AREAS","LIC_CORE","LIC_GEAR_SPEC","SPECIES_ISDB","SPECIES_MARFIS","LIC", "LICENCE_ID", "TRIP_ID_ISDB", "TRIP_ID_MARF", "VR", "VR_NUMBER_FISHING", "."))
