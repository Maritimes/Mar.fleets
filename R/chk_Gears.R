#' @title chk_Gears
#' @description The function looks at the values in the gear_desc field, and determines the "types"
#' of gears that are present (i.e. mesh/traps/line) as each of these types has distinct sizes that
#' go with them. e.g. mesh gear has a mesh size, and those sorts of values are different than the
#' sizes that might be associated with traps or hooks.
#' @param df dataframe
#' @noRd
chk_Gears <- function(df=df){
  allGears = tolower(unique(df$GEAR_DESC))
  allGears = allGears[!allGears %in% c("trap net")]
  matchTrap=c('trap','pot')
  matchMesh=c('trawl','seine','net','midwtr', 'drag')
  matchLine=c('line','jig','anli')
  theseGears<-NA
  if (any(grepl(pattern = paste(matchTrap, collapse = '|'), x= allGears))) theseGears <- c(theseGears,"trap")
  if (any(grepl(pattern = paste(matchMesh, collapse = '|'), x= allGears))) theseGears <- c(theseGears,"mesh")
  if (any(grepl(pattern = paste(matchLine, collapse = '|'), x= allGears))) theseGears <- c(theseGears,"line")
  theseGears <- theseGears[!is.na(theseGears)]
  if (length(theseGears)>1){
    gearType <-theseGears
  }else if (length(theseGears)==1){
    gearType <-theseGears
  }else{
    gearType <- NA
  }
  return(gearType)
}


