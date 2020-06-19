#' @title canRun
#' @description This function assesses the whether or not the package can accomodate a request to
#' run locally or directly against  oracle.  If the desire is to run locally, all of the necessary
#' tables must be available in the data.dir.  If run against Oracle, we must be on the network and
#' have access to the necessary schema.tables.
#' @noRd
canRun <- function(...){
  args=list(...)
  data.dir <- NA
  if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
  #for each vector below, [1] is the name of the schema
  OBS = c("ISDB.ISFISHSETS","ISDB.ISSETPROFILE_WIDE","ISDB.ISTRIPS","ISDB.ISVESSELS")
  MARFIS = c("MARFISSCI.GEARS","MARFISSCI.HAIL_IN_CALLS","MARFISSCI.HAIL_OUTS","MARFISSCI.LICENCE_SUBTYPES",
             "MARFISSCI.LICENCE_VESSELS","MARFISSCI.LICENCES","MARFISSCI.LOG_EFRT_ENTRD_DETS",
             "MARFISSCI.LOG_EFRT_STD_INFO","MARFISSCI.MON_DOC_DEFNS","MARFISSCI.MON_DOC_ENTRD_DETS",
             "MARFISSCI.MON_DOCS","MARFISSCI.NAFO_UNIT_AREAS","MARFISSCI.PRO_SPC_INFO","MARFISSCI.VESSELS")
  connect_Oracle <-function(...){
    args=list(...)$argsList
    if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
    cxn <- do.call(Mar.utils::make_oracle_cxn, list(usepkg = args$usepkg,
                                                    fn.oracle.username = args$oracle.username,
                                                    fn.oracle.password = args$oracle.password,
                                                    fn.oracle.dsn = args$oracle.dsn,
                                                    quietly = args$quiet))
    return(cxn)
  }
  tblAccess <- function(tables = NULL,...){
    args=list(...)$argsList

    #the schema holding the ISDB objects is actually observer
    tables <- gsub("ISDB","OBSERVER", tables)
    # #check for access
    fails = 0
    for (t in 1:length(tables)){
      if (!args$quiet) cat(paste0("Checking access to ",tables[t],": "))
      qry = paste0("select '1' from ", tables[t], " WHERE ROWNUM<=1")
      test = args$cxn$thecmd(args$cxn$channel, qry, rows_at_time = 1)
      if (is.character(test)) {
        fails=fails+1
        if (!args$quiet) cat(" failed","\n")
      }else{
        if (!args$quiet) cat(" success","\n")
      }
    }
    if (fails>0){
      return(FALSE)
    }else{
      return(TRUE)
    }
    #2check if has access
  }
  wantLocal <- function(tables = NULL, ...){
    args=list(...)$argsList
    if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")

    #1 check if local copies available '
    if (grepl(x = tables[1], pattern = "MARFIS")>0){
      data.dir <- NA
      #marfis is annoying because some files are prefaced with marfissci

      tabs1 = paste0(args$data.dir,.Platform$file.sep,tables,".RData")
      tabs2 = gsub(tabs1,pattern = "MARFISSCI", replacement = "MARFIS")
      localDataCheck1 <- sapply(X =tabs1, file.exists)
      localDataCheck2 <- sapply(X =tabs2, file.exists)
      mChk= data.frame(localDataCheck1, localDataCheck2)
      mChk$RES <- mChk$localDataCheck1 + localDataCheck2
      localDataCheck <- all(mChk$RES>0)
      rm(tabs1, tabs2, localDataCheck1, localDataCheck2, mChk)
    }else{
      data.dir <- NA
      tabs = paste0(args$data.dir,.Platform$file.sep,tables,".RData")
      localDataCheck <- all(sapply(X =tabs, file.exists))
      rm(tabs)
    }
    return(localDataCheck)
  }

  if (args$useLocal){
    if (do.call(wantLocal,list(MARFIS,argsList=args))&do.call(wantLocal,list(OBS,argsList=args))){
      return(TRUE)
    }else{
      cat(paste0("Cannot proceed offline. Check that all of the following files are in your data.dir (",args$data.dir,"):\n"))
      cat(paste0(MARFIS,".RData"),sep= "\n")
      cat(paste0(OBS,".RData"),sep= "\n")
      return(FALSE)
    }
  }else{

    cxnCheck <- do.call(connect_Oracle, list(argsList=args))
    if (!is.list(cxnCheck)) {
      cat("\nCan't create a DB connection.  \nPlease provide oracle.username, oracle.password, oracle.dsn (e.g. 'PTRAN') and usepkg (e.g.'roracle' or 'rodbc').\n")
      return(FALSE)
    }else{
      if (!args$quiet)  cat("\nDB connection established.\n")
      args[['cxn']] <- cxnCheck
    }
    if (all(do.call(tblAccess,list(MARFIS, argsList=args)) && do.call(tblAccess,list(OBS, argsList=args)))){
      return(cxnCheck)
    }else{
      return(FALSE)
    }
  }
}
