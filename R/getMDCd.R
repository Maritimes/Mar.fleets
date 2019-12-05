# Prompt for and/or Apply Md Code Filters ------------------------------------
getMDCd<-function(keep= keep, df = df, md = md){

  assign("mdDone", TRUE, envir = keep)
  mdDf = unique(df[,c("MD_DESC","MD_CODE")])
  mdDf = mdDf[with(mdDf,order(MD_CODE)),]
  if (any(md =="all")){
    MDCds = unique(mdDf$MD_CODE)
  } else if (length(md)>0){
    MDCds = mdDf[mdDf$MD_CODE %in% md,]
  } else{
    choice<-utils::select.list(paste0(mdDf$MD_DESC, " (",mdDf$MD_CODE,")"),
                               preselect=NULL,
                               multiple=T, graphics=T,
                               title='Monitoring Document Codes')
    cat("\n","mdCode choice: ",choice)
    choice = sub(".*\\((.*)\\).*", "\\1", choice)
    if ((choice=="" || is.na(choice)))stop("\n\nNo selection made - Aborting.")
    MDCds = mdDf[mdDf$MD_CODE %in% choice,]
  }
  return(MDCds)
}
