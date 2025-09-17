#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
.onAttach <- function(libname, pkgname) {
  Mar.utils::updateCheck(gitPkg = 'Maritimes/Mar.fleets')
  localVer = utils::packageDescription('Mar.fleets')$Version
  if (localVer >= "2025.06.05") {
         packageStartupMessage(
             "BREAKING CHANGES in this version! See README.md or NEWS.md for details.\n",
             "Key changes: Oracle connections required, data.dir removed, data encrypted.\n",
             "Timezone set to 'America/Halifax' to match data"
           )
       }
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
                         "HAIL_IN_CALLS","HAIL_OUTS", "PRO_SPC_INFO","TRIPS", ".",
                         "getFromNamespace","NAFO_MARF_SETS","LOG_EFRT_STD_INFO_ID",
                         "RND_WEIGHT_KGS","SPECIES_CODE","cxn"))
