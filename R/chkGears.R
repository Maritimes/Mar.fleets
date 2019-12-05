chkGears <- function(df=df){
  allGears = tolower(unique(df$GEAR_DESC))
  allGears = allGears[!allGears %in% c("trap net")]


  matchTrap=c('trap','pot')
  matchMesh=c('trawl','seine','net','midwtr')
  matchLine=c('line','jig','anli')
  theseGears<-NA
  if (grepl(pattern = paste(matchTrap, collapse = '|'), x= allGears)) theseGears <- c(theseGears,"trap")
  if (grepl(pattern = paste(matchMesh, collapse = '|'), x= allGears)) theseGears <- c(theseGears,"mesh")
  if (grepl(pattern = paste(matchLine, collapse = '|'), x= allGears)) theseGears <- c(theseGears,"line")
  theseGears <- theseGears[!is.na(theseGears)]
  if (length(theseGears)>1){
    browser()
  }else if (length(theseGears)==1){
    gearType <-theseGears
  }else{
    gearType <- NA
  }
  return(gearType)
}
