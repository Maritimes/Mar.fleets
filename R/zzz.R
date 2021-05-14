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

