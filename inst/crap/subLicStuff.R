
if (is.null(subLic)) subLic<- 'all'

if(!keep$subLicDone){
  if (length(subLic)>0 && subLic != 'all'){
    df<- getSubLic(cxn, keep, df, dateStart, dateEnd,  subLic, quietly)
  }else if (is.null(subLic)){
    allOptions <- c(allOptions, "Licence Subtype")
  }else if (subLic == 'all'){
    keep$subLicDone <- T
  }
}

if (choice=="Licence Subtype"){
  df<- getSubLic(cxn, keep, df, dateStart, dateEnd,  subLic, useDate, quietly)
  df <- applyFilters(cxn = cxn,
                     keep = keep,
                     quietly = quietly,
                     df=df,
                     mdCode=mdCode,
                     gearCode=gearCode,
                     nafoCode = nafoCode,
                     gearSpType = gearSpType,
                     gearSpSize = gearSpSize,
                     dateStart = dateStart,
                     dateEnd = dateEnd,
                     useDate = useDate,
                     noPrompts = noPrompts)
}

#' @param subLic default is \code{NULL}. This is the MARFIS Licence Subtype.If this is left as NULL, a popup
#' will allow the user to select one from a list. Valid codes are shown below:
#' \itemize{
#' \item 1 = "EXPLORATORY"
#' \item 2 = "COMMERCIAL COMMUNAL"
#' \item 3 = "CATEGORY A"
#' \item 4 = "CATEGORY B"
#' \item 5 = "PARTNERSHIP A"
#' \item 6 = "PARTNERSHIP B"
#' \item 7 = "PARTNERSHIP"
#' \item 8 = "DEVELOPMENT"
#' \item 9 = "EXPERIMENTAL"
#' \item 10	= "EDUCATIONAL"
#' \item 11	= "PUBLIC DISPLAY"
#' \item 12	= "OYSTER PICKING"
#' \item 13	= "OYSTER SPECIAL"
#' \item 14	= "MIDSHORE"
#' \item 15	= "OFFSHORE"
#' \item 16	= "MID BAY OF FUNDY SCALLOPS"
#' \item 17	= "UPPER BAY OF FUNDY SCALLOPS"
#' \item 18	= "FULL BAY OF FUNDY SCALLOPS"
#' \item 19	= "INSHORE"
#' \item 20	= "PREDATOR"
#' \item 21	= "LANDSMAN - PROFESSIONAL"
#' \item 22	= "LANDSMAN - ASSISTANT"
#' \item 23	= "OBSERVATION"
#' \item 24	= "FIXED GEAR GROUNDFISH <45'"
#' \item 25	= "TEMPORARY"
#' \item 26	= "TEMPORARY COMMUNAL"
#' \item 27	= "SILVER HAKE"
#' \item 28	= "FIXED GEAR 45-65 FEET - GROUNDFISH"
#' \item 29	= "GULF - TEST FISHERY"
#' \item 30	= "GULF - EXP COMMUNAL"
#' \item 31	= "GULF - COMMUNAL"
#' \item 32	= "GULF - OFFSHORE TEMPORARY"
#' \item 33	= "SNOW CRAB COMPANIES"
#' \item 34	= "ELVERS"
#' \item 35	= "SCIENTIFIC"
#' \item 36	= "MCFR"
#' \item 37	= "LIVE FISH TRANSFER"
#' \item 38	= "GENERAL TRANSFER LICENCE"
#' \item 39	= "NUISANCE"
#' \item 40	= "BROODSTOCK COLLECTION"
#' \item 41	= "FOREIGN VESSEL FISHING"
#' \item 42	= "FOREIGN VESSEL LOAD/OFFLOAD"
#' \item 43	= "FOREIGN VESSEL RESEARCH"
#' \item 44	= "PERSONAL USE"
#' \item 45	= "USE OF FISH - SECTION 10"
#' \item 46	= "REPLANTING LICENCE"
#' \item 47	= "DALIWHAL 10"
#' \item 48	= "TOBIN 10"
#' \item 49	= "PORCUPINE BROOK"
#' \item 50	= "INVASIVE SPECIES"
#' }
