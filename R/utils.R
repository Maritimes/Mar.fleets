#' @title chk_Gears
#' @description The function looks at the values in the gear_desc field, and determines the "types"
#' of gears that are present (i.e. mesh/traps/line) as each of these types has distinct sizes that
#' go with them. e.g. mesh gear has a mesh size, and those sorts of values are different than the
#' sizes that might be associated with traps or hooks.
#' @param df dataframe
#' @noRd
chk_Gears <- function(df=df){
  allGears = tolower(unique(df$GEAR_DESC))
  allGears = allGears[!allGears %in% c("trap net")]
  matchTrap=c('trap','pot')
  matchMesh=c('trawl','seine','net','midwtr', 'drag')
  matchLine=c('line','jig','anli')
  theseGears<-NA
  if (any(grepl(pattern = paste(matchTrap, collapse = '|'), x= allGears))) theseGears <- c(theseGears,"trap")
  if (any(grepl(pattern = paste(matchMesh, collapse = '|'), x= allGears))) theseGears <- c(theseGears,"mesh")
  if (any(grepl(pattern = paste(matchLine, collapse = '|'), x= allGears))) theseGears <- c(theseGears,"line")
  theseGears <- theseGears[!is.na(theseGears)]
  if (length(theseGears)>1){
    gearType <-theseGears
  }else if (length(theseGears)==1){
    gearType <-theseGears
  }else{
    gearType <- NA
  }
  return(gearType)
}
#' @title dbAccess
#' @description rolls up chk_OfflineData and chk_OracleAvail to check if the database is accessible,
#' and if all of the necessary data is available locally, returning TRUE or FALSE only.
#' @noRd
dbAccess<-function(data.dir = NULL){
  if(!chk_OracleAvail()){
    #No Oracle cxn.  Try local.
    f_chk <- chk_OfflineData(data.dir = data.dir, check = "get_fleet")
    m_chk <- chk_OfflineData(data.dir = data.dir, check = "get_marfis")
    o_chk <- chk_OfflineData(data.dir = data.dir, check = "get_obs")
    g_chk <- chk_OfflineData(data.dir = data.dir, check = "get_gearspecs")

    allChk <- c(f_chk, m_chk, o_chk, g_chk)
    if (!all(allChk))stop("No Oracle, and no offline files. Stopping")
    #cat("Oracle unavailable, but have local files. Proceeding.")
    return(FALSE)
  }else{
    cat("Oracle exists.  use that.")
    return(TRUE)
  }
}
#' @title chk_OracleAvail
#' @description This sees if the script can find Oracle.  If connected to the maritimesdfo network,
#' it should be available, but if working from home, it won't be.  This will be used to help decide
#' whether to proceed with sql queries to the db, or use of offline files
#' @param url_in url to try to hit to verify network access
#' @param t the time in seconds before timeout
#' @noRd
chk_OracleAvail <- function(url_in = "http://vsnsbioxp74.ent.dfo-mpo.ca/",t=2){
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
  suppressWarnings(try(close.connection(con),silent=T))
  ifelse(is.null(check),TRUE,FALSE)
}
#' @title chk_OfflineData
#' @description This function verifies that all of the necessary local files exist before embarking
#' on an attempt to extract data from local sources
#' @param data.dir place where the data lives
#' @param check a parameter that identifies which functions will be run (and therefore which
#' datasets to look for)
#' @noRd
chk_OfflineData <- function(data.dir = NULL, check = NULL){
  check = tolower(check)
  tables = switch(check,
                  "get_fleet" = c(file.path(data.dir,"MARFIS.GEARS.rdata"),
                                  file.path(data.dir,"MARFIS.LICENCE_SUBTYPES.rdata"),
                                  file.path(data.dir,"MARFIS.LICENCES.rdata"),
                                  file.path(data.dir,"MARFIS.LOG_EFRT_ENTRD_DETS.rdata"),
                                  file.path(data.dir,"MARFIS.LOG_EFRT_STD_INFO.rdata"),
                                  file.path(data.dir,"MARFIS.MON_DOCS.rdata"),
                                  file.path(data.dir,"MARFIS.MON_DOC_DEFNS.rdata"),
                                  file.path(data.dir,"MARFIS.NAFO_UNIT_AREAS.rdata"),
                                  file.path(data.dir,"MARFIS.PRO_SPC_INFO.rdata"),
                                  file.path(data.dir,"MARFIS.VESSELS.rdata")),
                  "get_marfis" = c(file.path(data.dir,"MARFIS.LOG_EFRT_STD_INFO.rdata"),
                                   file.path(data.dir,"MARFIS.PRO_SPC_INFO.rdata"),
                                   file.path(data.dir,"MARFIS.NAFO_UNIT_AREAS.rdata"),
                                   file.path(data.dir,"MARFIS.VESSELS.rdata"),
                                   file.path(data.dir,"MARFIS.MON_DOC_ENTRD_DETS.rdata"),
                                   file.path(data.dir,"MARFIS.HAIL_IN_CALLS.rdata"),
                                   file.path(data.dir,"MARFIS.HAIL_OUTS.rdata")),
                  "get_gearspecs" = c(file.path(data.dir,"MARFIS.LOG_EFRT_STD_INFO.rdata"),
                                      file.path(data.dir,"MARFIS.LOG_EFRT_ENTRD_DETS.rdata")),
                  "get_obs" = c(file.path(data.dir,"ISDB.ISTRIPS.rdata"),
                                file.path(data.dir,"ISDB.ISFISHSETS.rdata"),
                                file.path(data.dir,"ISDB.ISSETPROFILE_WIDE.rdata"),
                                file.path(data.dir,"ISDB.ISVESSELS.rdata"),
                                file.path(data.dir,"MARFIS.LICENCE_VESSELS.rdata"))
  )
  localDataCheck <- all(sapply(X =tables, file.exists))
  if (localDataCheck){
    #cat("Access to all required local data confirmed.  Proceeding...\n")
  }else{
    cat("Cannot proceed offline. Check that all of the following files are in your data.dir:\n")
    cat(paste(tables),sep= "\n")
  }
  return(localDataCheck)
}


