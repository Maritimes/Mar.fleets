#' @title canRun
#' @description This function assesses the whether or not the package can accomodate a request to
#' run locally or directly against  oracle.  If the desire is to run locally, all of the necessary
#' tables must be available in the data.dir.  If run against Oracle, we must be on the network and
#' have access to the necessary schema.tables.
#' @noRd
canRun <- function(useLocal = T, ...){
  #for each vector below, [1] is the name of the schema
  OBS = c("ISDB","ISFISHSETS","ISSETPROFILE_WIDE","ISTRIPS","ISVESSELS")
  MARFIS = c("MARFISSCI","GEARS","HAIL_IN_CALLS","HAIL_OUTS","LICENCE_SUBTYPES",
             "LICENCE_VESSELS","LICENCES","LOG_EFRT_ENTRD_DETS",
             "LOG_EFRT_STD_INFO","MON_DOC_DEFNS","MON_DOC_ENTRD_DETS",
             "MON_DOCS","NAFO_UNIT_AREAS","PRO_SPC_INFO","VESSELS")

  wantRemote <- function(...){
    #1check if on network
    chk_OracleAvail <- function(url_in = "https://intranet.ent.dfo-mpo.ca/",t=2){
      con <- url(url_in)
      check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
      this <- ifelse(is.null(check),TRUE,FALSE)
      suppressWarnings(close.connection(con))
      rm(con)
      return(this)
    }

    if(!chk_OracleAvail()){
      cat("You don't appear to be on the network")
      return(invisible(FALSE))
    }else{
      # #check for access
      # qry = paste0("select '1' from ", theschema, ".",
      #              gsub(".*\\.", "", tables), " WHERE ROWNUM<=1")
      # if (is.character(thecmd(oracle_cxn, qry, rows_at_time = 1))) {
      #   if (!quiet) cat(" failed")
      #   return(FALSE)
      # }
      # else {
      #   if (!quiet) cat(" success")
      #   return(TRUE)
      # }
      #to check tables, I will need to ensure that teh schema.table nomenclature is consistent -
      #no calling marfissci marfis and vice versa
      cat("You appear to be on the network.  \n
In the future, this will also verify you have access to the necessary tables.")
      return(invisible(TRUE))
    }
    #2check if has access
  }

  wantLocal <- function(tables = NULL, ...){
    #1 check if local copies available '
    if (grepl(x = tables[1], pattern = "MARFIS")>0){
      #marfis is annoying because some files are prefaced with marfissci
      tabs1 = paste0(data.dir,.Platform$file.sep,"MARFIS",".",tables[2:length(tables)],".RData")
      tabs2 = paste0(data.dir,.Platform$file.sep,"MARFISSCI",".",tables[2:length(tables)],".RData")
      localDataCheck1 <- sapply(X =tabs1, file.exists)
      localDataCheck2 <- sapply(X =tabs2, file.exists)
      mChk= data.frame(localDataCheck1, localDataCheck2)
      mChk$RES <- mChk$localDataCheck1 + localDataCheck2
      localDataCheck <- all(mChk$RES>0)
    }else{
      tabs = paste0(data.dir,.Platform$file.sep,tables[1],".",tables[2:length(tables)],".RData")
      localDataCheck <- all(sapply(X =tabs, file.exists))
    }
    return(localDataCheck)
  }

  if (useLocal){
  if (wantLocal(MARFIS)&wantLocal(OBS)){
    return(invisible(TRUE))
  }else{
    cat("Cannot proceed offline. Check that all of the following files are in your data.dir:\n")
    cat(paste(tables),sep= "\n")
    return(invisible(FALSE))
  }
  }else{
    tt = wantRemote(...)
  }

}

#' tt = main(useLocal = F, data.dir = data.dir)
#' ## 1)choice
#' ### useLocal
#' ####    check if have local copies
#' ####    chk_OfflineData
#' ####    dbAccess() #modified
#' ### useRemote
#' ####    check if Oracle available
#' ####    chk_OracleAvail()
#'
#' #' @title dbAccess
#' #' @description rolls up chk_OfflineData and chk_OracleAvail to check if the database is accessible,
#' #' and if not, verifies that all of the necessary data is available locally, returning TRUE or FALSE
#' #'  only.
#' #' @noRd
#' dbAccess<-function(data.dir = NULL){
#'   if(!chk_OracleAvail()){
#'     #No Oracle cxn.  Try local.
#'     f_chk <- chk_OfflineData(data.dir = data.dir, check = "get_fleet")
#'     m_chk <- chk_OfflineData(data.dir = data.dir, check = "get_marfis")
#'     o_chk <- chk_OfflineData(data.dir = data.dir, check = "get_obs")
#'     g_chk <- chk_OfflineData(data.dir = data.dir, check = "get_gearspecs")
#'
#'     allChk <- c(f_chk, m_chk, o_chk, g_chk)
#'     if (!all(allChk))stop("No Oracle, and no offline files. Stopping")
#'     #cat("Oracle unavailable, but have local files. Proceeding.")
#'     return(FALSE)
#'   }else{
#'     cat("Oracle exists.  use that.")
#'     return(TRUE)
#'   }
#' }
#' #' @title chk_OracleAvail
#' #' @description This script checks to see if the the computer running the analysis is on  the dfo
#' #' network simply by hitting an intranet site. This is used to help decide whether to proceed with
#' #' sql queries to the db, or use of offline files
#' #' @param url_in url to try to hit to verify network access
#' #' @param t the time in seconds before timeout
#' #' @noRd
#' chk_OracleAvail <- function(url_in = "https://intranet.ent.dfo-mpo.ca/",t=2){
#'   con <- url(url_in)
#'   check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
#'   this <- ifelse(is.null(check),TRUE,FALSE)
#'   suppressWarnings(close.connection(con))
#'   rm(con)
#'   return(this)
#' }
#' #' @title chk_OfflineData
#' #' @description This function verifies that all of the necessary local files exist before embarking
#' #' on an attempt to extract data from local sources
#' #' @param data.dir place where the data lives
#' #' @param check a parameter that identifies which functions will be run (and therefore which
#' #' datasets to look for)
#' #' @noRd
#' chk_OfflineData <- function(data.dir = NULL, check = NULL){
#'   check = tolower(check)
#'   tables = switch(check,
#'                   "get_fleet" = c(file.path(data.dir,"MARFIS.GEARS.RData"),
#'                                   file.path(data.dir,"MARFIS.LICENCE_SUBTYPES.RData"),
#'                                   file.path(data.dir,"MARFIS.LICENCES.RData"),
#'                                   file.path(data.dir,"MARFIS.LOG_EFRT_ENTRD_DETS.RData"),
#'                                   file.path(data.dir,"MARFIS.LOG_EFRT_STD_INFO.RData"),
#'                                   file.path(data.dir,"MARFIS.MON_DOCS.RData"),
#'                                   file.path(data.dir,"MARFIS.MON_DOC_DEFNS.RData"),
#'                                   file.path(data.dir,"MARFIS.NAFO_UNIT_AREAS.RData"),
#'                                   file.path(data.dir,"MARFIS.PRO_SPC_INFO.RData"),
#'                                   file.path(data.dir,"MARFIS.VESSELS.RData")),
#'                   "get_marfis" = c(file.path(data.dir,"MARFIS.LOG_EFRT_STD_INFO.RData"),
#'                                    file.path(data.dir,"MARFIS.PRO_SPC_INFO.RData"),
#'                                    file.path(data.dir,"MARFIS.NAFO_UNIT_AREAS.RData"),
#'                                    file.path(data.dir,"MARFIS.VESSELS.RData"),
#'                                    file.path(data.dir,"MARFIS.MON_DOC_ENTRD_DETS.RData"),
#'                                    file.path(data.dir,"MARFIS.HAIL_IN_CALLS.RData"),
#'                                    file.path(data.dir,"MARFIS.HAIL_OUTS.RData")),
#'                   "get_gearspecs" = c(file.path(data.dir,"MARFIS.LOG_EFRT_STD_INFO.RData"),
#'                                       file.path(data.dir,"MARFIS.LOG_EFRT_ENTRD_DETS.RData")),
#'                   "get_obs" = c(file.path(data.dir,"ISDB.ISTRIPS.RData"),
#'                                 file.path(data.dir,"ISDB.ISFISHSETS.RData"),
#'                                 file.path(data.dir,"ISDB.ISSETPROFILE_WIDE.RData"),
#'                                 file.path(data.dir,"ISDB.ISVESSELS.RData"),
#'                                 file.path(data.dir,"MARFIS.LICENCE_VESSELS.RData"))
#'   )
#'   localDataCheck <- all(sapply(X =tables, file.exists))
#'   if (localDataCheck){
#'     #cat("Access to all required local data confirmed.  Proceeding...\n")
#'   }else{
#'     cat("Cannot proceed offline. Check that all of the following files are in your data.dir:\n")
#'     cat(paste(tables),sep= "\n")
#'   }
#'   return(localDataCheck)
#' }
#'
