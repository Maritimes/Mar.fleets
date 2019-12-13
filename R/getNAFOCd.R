# Prompt for and/or Apply NAFO Filters ------------------------------------
getNAFOCd<-function(keep= keep, df = df, nafoCode = nafoCode){
  # assign("gearDone", TRUE, envir = keep)
  keep$nafoDone <- T
  nDf = unique(df["NAFO"])
  nDf = nDf[order(nDf$NAFO), ,drop = FALSE]
  if (any(nafoCode =="all")){
    NCds = unique(nDf$NAFO)
  } else if (length(nafoCode)>0){
    NCds = nDf[nDf$NAFO %in% nafoCode,]
  }else{
    choice<-utils::select.list(nDf$NAFO,
                               preselect=NULL,
                               multiple=T, graphics=T,
                               title='NAFO Codes')
    cat(paste0("\n","nafoCode choice: ",paste0(choice, collapse=",")))
    # choice = sub(".*\\((.*)\\).*", "\\1", choice)
    if ((choice=="" || is.na(choice)))stop("\n\nNo selection made - Aborting.")
    NCds = nDf[nDf$NAFO %in% choice,]
  }
  return(NCds)
}
