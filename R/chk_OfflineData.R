dbAccess<-function(){
  if(!chk_OracleAvail()){
  #No Oracle cxn.  Try local.
  f_chk <- chk_OfflineData(data.dir = data.dir, check = "get_fleet")
  m_chk <- chk_OfflineData(data.dir = data.dir, check = "get_marfis")
  o_chk <- chk_OfflineData(data.dir = data.dir, check = "get_obs")

  allChk <- c(f_chk, m_chk, o_chk)
  if (!all(allChk))stop("No Oracle, and no offline files. Stopping")
  #cat("Oracle unavailable, but have local files. Proceeding.")
  return(FALSE)
}else{
  cat("Oracle exists.  use that.")
  return(TRUE)
}
}

chk_OracleAvail <- function(url_in = "http://vsnsbioxp74.ent.dfo-mpo.ca/",t=2){
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
  suppressWarnings(try(close.connection(con),silent=T))
  ifelse(is.null(check),TRUE,FALSE)
}

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


