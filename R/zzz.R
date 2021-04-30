#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
.onAttach <- function(libname, pkgname) {
  assign("dbEnv", new.env(), envir = .GlobalEnv)
  Mar.utils::updateCheck(gitPkg = 'Maritimes/Mar.bycatch')
  localVer = utils::packageDescription('Mar.bycatch')$Version
  packageStartupMessage(paste0("Version: ", localVer))
}

.onLoad <- function(libname, pkgname){
  options(stringsAsFactors = FALSE)
  Sys.setenv(TZ = "America/Halifax")
}

