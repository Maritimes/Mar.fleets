#' @title canRun
#' @description This function assesses the whether or not the package can accomodate a request to
#' run locally or directly against  oracle.  If the desire is to run locally, all of the necessary
#' tables must be available in the data.dir.  If run against Oracle, we must be on the network and
#' have access to the necessary schema.tables.
#' @noRd
canRun <- function(...){
  args=list(...)
  if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
  #for each vector below, [1] is the name of the schema
  OBS = c("ISDB.ISFISHSETS","ISDB.ISSETPROFILE_WIDE","ISDB.ISTRIPS","ISDB.ISVESSELS")
  MARFIS = c("MARFISSCI.GEARS","MARFISSCI.HAIL_IN_CALLS","MARFISSCI.HAIL_OUTS","MARFISSCI.LICENCE_SUBTYPES",
             "MARFISSCI.LICENCE_VESSELS","MARFISSCI.LICENCES","MARFISSCI.LOG_EFRT_ENTRD_DETS",
             "MARFISSCI.LOG_EFRT_STD_INFO","MARFISSCI.MON_DOC_DEFNS","MARFISSCI.MON_DOC_ENTRD_DETS",
             "MARFISSCI.MON_DOCS","MARFISSCI.NAFO_UNIT_AREAS","MARFISSCI.PRO_SPC_INFO","MARFISSCI.VESSELS")

  wantRemote <- function(...){
    args=list(...)
    if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
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
      return(FALSE)
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
      return(TRUE)
    }
    #2check if has access
  }

  wantLocal <- function(tables = NULL, ...){
    args=list(...)
    if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
    #1 check if local copies available '
    if (grepl(x = tables[1], pattern = "MARFIS")>0){
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
      tabs = paste0(args$data.dir,.Platform$file.sep,tables,".RData")
      localDataCheck <- all(sapply(X =tabs, file.exists))
      rm(tabs)
    }
    return(localDataCheck)
  }

  if (args$useLocal){
    if (wantLocal(MARFIS,...)&wantLocal(OBS,...)){
      return(TRUE)
    }else{
      cat(paste0("Cannot proceed offline. Check that all of the following files are in your data.dir (",args$data.dir,"):\n"))
      cat(paste0(MARFIS,".RData"),sep= "\n")
      cat(paste0(OBS,".RData"),sep= "\n")
      return(FALSE)
    }
  }else{
    tt = wantRemote(...)
  }
}
