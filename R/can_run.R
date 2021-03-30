#' @title can_run
#' @description This function assesses the whether or not the package can accomodate a request to
#' run locally or directly against  oracle.  If the desire is to run locally, all of the necessary
#' tables must be available in the data.dir.  If run against Oracle, we must be on the network and
#' have access to the necessary schema.tables.
#' @family setup
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @noRd
can_run <- function(...){
  args=list(...)
  if (args$debuggit){
    Mar.utils::where_now()
  }

  connect_Oracle <-function(...){
    #connect_Oracle is just results of trying to establish connection
    args=list(...)$args
    cxn <- do.call(Mar.utils::make_oracle_cxn, list(usepkg = args$usepkg,
                                                    fn.oracle.username = args$oracle.username,
                                                    fn.oracle.password = args$oracle.password,
                                                    fn.oracle.dsn = args$oracle.dsn,
                                                    quietly = args$quietly))
    return(cxn)
  }

  tblAccess <- function(tables = NULL,...){
    #tblAccess is T if has necess permiss
    args=list(...)$args
    tables <- gsub("ISDB","OBSERVER", tables)
    fails = 0
    for (t in 1:length(tables)){
      if (!args$quietly)message(paste0("Checking access to ",tables[t],": "))
      qry = paste0("select '1' from ", tables[t], " WHERE ROWNUM<=1")
      test = args$cxn$thecmd(args$cxn$channel, qry, rows_at_time = 1)
      if (is.character(test)) {
        fails=fails+1
        if (!args$quietly)message(" failed","\n")
      }else{
        if (!args$quietly)message(" success","\n")
      }
    }
    if (fails>0){
      return(FALSE)
    }else{
      return(TRUE)
    }
  }

  wantLocal <- function(tables = NULL, ...){
    #wantLocal is T if all necessar things are found
    args<-list(...)$args
    #1 check if local copies available '
    if (grepl(x = tables[1], pattern = "MARFIS")>0){
      tabs1 = paste0(args$data.dir,.Platform$file.sep,tables,".RData")
      tabs2 = gsub(tabs1,pattern = "MARFISSCI", replacement = "MARFIS")
      localDataCheck1 <- sapply(X =tabs1, file.exists)
      localDataCheck2 <- sapply(X =tabs2, file.exists)
      mChk= data.frame(localDataCheck1, localDataCheck2)
      mChk$RES <- mChk$localDataCheck1 + localDataCheck2
      localDataCheck <- all(mChk$RES>0)
      rm(tabs1, tabs2, localDataCheck1, localDataCheck2, mChk)
    }else{
      tabs = paste0(args$data.dir,.Platform$file.sep,tables,".RData")
      localDataCheck <- all(sapply(X =tabs, file.exists))
      rm(tabs)
    }
    return(localDataCheck)
  }

  marfTabs = c("HAIL_IN_CALLS",
               "HAIL_OUTS",
               "LOG_EFRT_ENTRD_DETS",
               "LOG_EFRT_STD_INFO",
               "MARBYCATCH_LIC",
               "MON_DOC_ENTRD_DETS",
               "NAFO_UNIT_AREAS",
               "PRO_SPC_INFO",
               "TRIPS",
               "VESSELS",
               "GEARS")

  isdbTabs = c("ISFISHSETS",
               "ISSETPROFILE_WIDE",
               "ISTRIPS",
               "ISVESSELS")

  args[["marfTabs"]] <- marfTabs
  args[["isdbTabs"]] <- isdbTabs
  ISDB = paste0("ISDB.",isdbTabs)
  MARFIS = paste0("MARFISSCI.",marfTabs)

  if (args$useLocal){
    if (do.call(wantLocal,list(MARFIS,args=args)) & do.call(wantLocal,list(ISDB,args=args))){
      args[['cxn']] <- TRUE
      res <- args
      return(res)
    }else{
      message(paste0("Cannot proceed offline. Check that all of the following files are in your data.dir (",args$data.dir,"):\n"))
      message(paste0(MARFIS,".RData"),sep= "\n")
      message(paste0(ISDB,".RData"),sep= "\n")
      stop()
    }
  }else{
    cxnCheck <- do.call(connect_Oracle, list(args=args))
    if (!is.list(cxnCheck)) {
      message("\n","Cannot proceed online (Can't create a DB connection).",
          "\n","Please provide oracle.username, oracle.password, oracle.dsn (e.g. 'PTRAN') and usepkg (e.g.'roracle' or 'rodbc').","\n")
      stop()
      # return(FALSE)
    }else{
      if (!args$quietly)  message("\nDB connection established.\n")
      args[['cxn']] <- cxnCheck
      res <- args
      return(res)
    }
    if (all(do.call(tblAccess,list(MARFIS, args=args)) && do.call(tblAccess,list(ISDB, args=args)))){
      message("\n","Connected to DB, and verified that account has sufficient permissions to proceed.","\n")
      res <- args
      return(res)
    }else{
      message("\n","Connected to DB, but account does not have sufficient permissions to proceed.","\n")
           stop()
      #return(FALSE)
    }
  }
}
