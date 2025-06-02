#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
.onAttach <- function(libname, pkgname) {
  Mar.utils::updateCheck(gitPkg = 'Maritimes/Mar.fleets')
  localVer = utils::packageDescription('Mar.fleets')$Version
  packageStartupMessage(paste0("Version: ", localVer))
}

.onLoad <- function(libname, pkgname){
  options(stringsAsFactors = FALSE)
  Sys.setenv(TZ = "America/Halifax")
  base_dir <- file.path("C:", "DFO-MPO")
  pesd_fl_dir <- file.path(base_dir, "PESDData","MarFleets")
  if (!dir.exists(pesd_fl_dir)) dir.create(pesd_fl_dir, recursive = T)
}

utils::globalVariables(c("GEARS_MARFIS", "GEARS_ISDB","SPECIES_MARFIS","SPECIES_ISDB",
                         "LIC_AREAS","LIC_CORE","LIC_GEAR_SPEC",
                         "LIC", "LICENCE_ID", "TRIP_ID_ISDB", "TRIP_ID_MARF",
                         "VESSELS","VR", "VR_NUMBER_FISHING", "MON_DOC_ENTRD_DETS",
                         "HAIL_IN_CALLS","HAIL_OUTS", "PRO_SPC_INFO","TRIPS", "."))
