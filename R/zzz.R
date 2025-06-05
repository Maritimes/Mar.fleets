#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
.onAttach <- function(libname, pkgname) {
  Mar.utils::updateCheck(gitPkg = 'Maritimes/Mar.fleets')
  localVer = utils::packageDescription('Mar.fleets')$Version
  packageStartupMessage(paste0("Version: ", localVer))
  msg <- paste(
    #paste0("Version: ", localVer),
    "Timezone set to 'America/Halifax' to match data",
    "\n\n!! IMPORTANT:  Security Update - Protected B Data Encryption !!!",
    "Protected B data extracted by this package (i.e. MARFIS;ISDB) has been found on unrestricted network drives, which violates security protocols. These datasets carry responsibilities that were agreed to when your account was granted permissions.",
    "\nKey changes implemented:",
    "1) 'data.dir' is now deprecated; all extracted data will be stored in 'C:\\DFO-MPO\\PESDData\\MarFleets'",
    "2) Protected B data will now be encrypted when extracted (unclassified data will remain unencrypted)",
    "\nTo access Protected B Rdata files as a different user or on a different computer than the one that performed the extraction, you will need:",
    "\t'extract_user': The original extractor's username (from Sys.info()['user'])",
    "\t'extract_computer': The original computer name (from Sys.info()['nodename'])",
    "\nExamples:",
    "\tcxn <- ROracle::dbConnect(DBI::dbDriver('Oracle'), 'oracle.username', 'oracle.password', 'PTRAN')",
    "\tthis<- fleet_halibut(useLocal = T, cxn=cxn, year = 2021, socks = T )",
    "\t(or, if you're loading data extracted by someone else....)",
    "\tthis<- fleet_halibut(useLocal = T, cxn=cxn, year = 2021, socks = T, extract_user= 'the_users_name', extract_computer = 'the_users_computer' )",
    "\nWARNING: Any unencrypted Protected B data extracted by this package found on unsecured network drives will be deleted immediately.",
    "For questions or assistance, please contact Mike.McMahon@dfo-mpo.gc.ca",
    sep = "\n"
  )
  packageStartupMessage(msg)
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
