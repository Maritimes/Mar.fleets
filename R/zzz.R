.onAttach <- function(libname, pkgname) {
  localVer = utils::packageDescription('Mar.bycatch')$Version
  packageStartupMessage(paste0("Version: ", localVer))
}
.onLoad <- function(libname, pkgname){
  options(stringsAsFactors = FALSE)
  Sys.setenv(TZ = "America/Halifax")
  Mar.utils::updateCheck(gitPkg = 'Maritimes/Mar.bycatch')
}
