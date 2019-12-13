# Prompt for and/or Apply Gear Filters ------------------------------------
getGCd<-function(keep= keep, df = df, gearCode = gearCode){
  # assign("gearDone", TRUE, envir = keep)
  keep$gearDone <- T
  gDf = unique(df[,c("GEAR_DESC","GEAR_CODE")])
  gDf = gDf[with(gDf,order(GEAR_CODE)),]
  if (any(gearCode =="all")){
    GCds = unique(gDf$GEAR_CODE)
  } else if (length(gearCode)>0){
    GCds = gDf[gDf$GEAR_CODE %in% gearCode,]
  }else{
    choice<-utils::select.list(paste0(gDf$GEAR_DESC, " (",gDf$GEAR_CODE,")"),
                               preselect=NULL,
                               multiple=T, graphics=T,
                               title='Gear Codes')
    cat(paste0("\n","gearCode choice: ",paste0(choice, collapse=",")))
    choice = sub(".*\\((.*)\\).*", "\\1", choice)
    if ((choice=="" || is.na(choice)))stop("\n\nNo selection made - Aborting.")
    GCds = gDf[gDf$GEAR_CODE %in% choice,]
  }
  return(GCds)
}
