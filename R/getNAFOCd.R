# Prompt for and/or Apply NAFO Filters ------------------------------------
getNAFOCd<-function(keep= keep, df = df, nafoCode = nafoCode, quietly = F){
  keep$nafoDone <- TRUE
  nDf = unique(df["NAFO"])
  nDf = nDf[order(nDf$NAFO), ,drop = FALSE]
  if (any(nafoCode =="all")){
    NCds = unique(nDf$NAFO)
  }else if (is.null(nafoCode)) {
    choice<-utils::select.list(nDf$NAFO,
                               preselect=NULL,
                               multiple=T, graphics=T,
                               title='NAFO Codes')
    if (choice == "" || length(choice)<1 || is.na(choice)){
      keep$nafoDone <- FALSE
      return(df)
    }
    if (!quietly)cat(paste0("\n","nafoCode choice: ",paste0(choice, collapse=",")))
    NCds = nDf[nDf$NAFO %in% choice,]
  }else{
    #if nafocodes were provided, filtering shoul have occurred in basicFleet()
    cat(paste0("\n","Should never see this"))
  }
  return(NCds)
}
