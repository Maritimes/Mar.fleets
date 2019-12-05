#' @title get_fleet
#' @description This function extracts all of the Vessel/Licence combinations
#' associated with a particular fleet for a particular date range.
#' @param fn.oracle.username default is \code{'_none_'} This is your username for
#' accessing oracle objects. If you have a value for \code{oracle.username}
#' stored in your environment (e.g. from an rprofile file), this can be left out
#' and that value will be used.  If a value for this is provided, it will take
#' priority over your existing value.
#' @param fn.oracle.password default is \code{'_none_'} This is your password for
#' accessing oracle objects. If you have a value for \code{oracle.password}
#' stored in your environment (e.g. from an rprofile file), this can be left out
#' and that value will be used.  If a value for this is provided, it will take
#' priority over your existing value.
#' @param fn.oracle.dsn default is \code{'_none_'} This is your dsn/ODBC
#' identifier for accessing oracle objects. If you have a value for
#' \code{oracle.dsn} stored in your environment (e.g. from an rprofile file),
#' this can be left and that value will be used.  If a value for this is
#' provided, it will take priority over your existing value.
#' @param usepkg default is \code{'rodbc'}. This indicates whether the connection to Oracle should
#' use \code{'rodbc'} or \code{'roracle'} to connect.  rodbc is slightly easier to setup, but
#' roracle will extract data ~ 5x faster.
#' @param dateStart default is \code{NULL}. This is the start date (YYYY-MM-DD)
#' of the window of time you want to look at.
#' @param dateEnd default is \code{NULL}. This is the end date (YYYY-MM-DD)
#' of the window of time you want to look at.  If this is left blank, 1 year of
#' data will be returned.
#' @param mdCode default is \code{NULL}. This is the MARFIS Monitoring
#' Document code used to identify fleets. If this is left as NULL, a popup
#' will allow the user to select one from a list.
#' Valid codes are shown below:
#' \itemize{
#' \item 1 = 'FIXED GEAR GROUNDFISH'
#' \item 2 = 'MOBILE GEAR GROUNDFISH'
#' \item 3 = 'ATLANTIC BLUEFIN TUNA'
#' \item 4 = 'SWORDFISH HARPOON'
#' \item 5 = 'SWORDFISH/SHARK LONGLINE'
#' \item 6 = 'FIXED GEAR HERRING'
#' \item 7 = 'HERRING MOBILE GEAR'
#' \item 8 = 'TRANSPORT'
#' \item 9 = 'MACKEREL - FIXED GEAR'
#' \item 10 = 'CRAB'
#' \item 11 = 'EXPLORATORY ROCK/JONAH CRAB'
#' \item 12 = 'INSHORE DREDGE SHELLFISH'
#' \item 13 = 'OFFSHORE CLAM'
#' \item 14 = 'SCALLOP'
#' \item 15 = 'OFFSHORE SCALLOP'
#' \item 16 = 'MOBILE SHRIMP'
#' \item 17 = 'WHELK/MOONSNAIL'
#' \item 18 = 'SEA URCHIN'
#' \item 19 = 'OFFSHORE LOBSTER'
#' \item 26 = 'TEST'
#' \item 27 = 'SAINT JOHN RIVER ATLANTIC STURGEON FISHERY'
#' \item 28 = 'SHRIMP TRAP'
#' \item 29 = 'INTERNATIONAL'
#' \item 30 = 'DECK AND'
#' \item 31 = 'NORTHERN SHRIMP SLIP - CONVERSION'
#' \item 32 = 'NATIONAL SEA SLIP'
#' \item 33 = 'ELVER FISHERY'
#' \item 39 = 'SEA CUCUMBER'
#' \item 40 = 'TEST BLANK'
#' \item 41 = 'SEA CUCUMBER'
#' \item 46 = 'GENERIC CATCH ENTRY'
#' \item 47 = 'HAGFISH'
#' \item 48 = 'COMMERCIAL OYSTER FISHERY'
#' \item 49 = 'LOBSTER'
#' \item 50 = 'SHRIMP TRANSPORT'
#' \item 52 = 'SEA URCHIN'
#' \item 53 = 'SCALLOP DIVE'
#' }
#' @param gearCode default is \code{NULL}. In some cases, a fleet will contain multiple
#' gear types. Setting this to \code{NULL} (the default) will prompt you to
#' select gears from the available values (if there are multiple).  Setting it to
#' \code{'all'} will get all gear types.  Entering a vector of MARFIS gear codes
#' (e.g. \code{c(51,81)}) will only return those gear codes.
#' Valid codes are shown below:
#' \itemize{
#' \item 0 = 'UNKNOWN'
#' \item 10 = 'OTTER TRAWL'
#' \item 11 = 'OTTER TRAWL, SIDE'
#' \item 12 = 'OTTER TRAWL, STERN'
#' \item 13 = 'MIDWATER TRAWL'
#' \item 14 = 'MIDWATER TRAWL, SIDE'
#' \item 15 = 'MIDWATER TRAWL STERN'
#' \item 16 = 'OTTER TRAWL, PAIR'
#' \item 17 = 'MIDWATER TRAWL, PAIR'
#' \item 18 = 'SEMI PELAGIC TRAWL'
#' \item 19 = 'SHRIMP TRAWL'
#' \item 20 = 'MODIFY SHRIMP MIDWTR'
#' \item 21 = 'DANISH SEINE'
#' \item 22 = 'SCOTTISH SEINE'
#' \item 24 = 'BEACH/DRAG/BAR SEINE'
#' \item 31 = 'PURSE SEINE'
#' \item 32 = 'LAMPARA'
#' \item 33 = 'PAIR SEINE'
#' \item 41 = 'GILL NET (SET OR FIXED)'
#' \item 42 = 'GILL NET, DRIFT'
#' \item 43 = 'GILL NET'
#' \item 44 = 'SQUARE NET'
#' \item 45 = 'BOX NET'
#' \item 46 = 'BAG NET'
#' \item 47 = 'FYKE NET'
#' \item 51 = 'LONGLINE'
#' \item 52 = 'AUTOMATED JIGGER GROUNDFISH'
#' \item 53 = 'JIGGER'
#' \item 54 = 'TENDED LINE'
#' \item 55 = 'MECHANICAL JIGGER'
#' \item 59 = 'HAND LINE'
#' \item 60 = 'ANGLING'
#' \item 61 = 'TRAP NET'
#' \item 62 = 'TRAP'
#' \item 63 = 'WEIR'
#' \item 64 = 'STATIONARY LIFT NETS'
#' \item 65 = 'BOW NET'
#' \item 70 = 'DIP NET'
#' \item 71 = 'DRAG'
#' \item 72 = 'DREDGE, HAND'
#' \item 73 = 'MECHANICAL DIGGER'
#' \item 74 = 'HYDRAULIC DEVICE'
#' \item 75 = 'DIVING'
#' \item 76 = 'DIVER'
#' \item 78 = 'WING (DIP NET)'
#' \item 81 = 'HARPOON/SPEAR'
#' \item 82 = 'SEAL HUNTING'
#' \item 83 = 'SPEAR'
#' \item 84 = 'EEL POT'
#' \item 85 = 'ELECTRIC HARPOON'
#' \item 90 = 'MISCELLANEOUS'
#' \item 91 = 'RAKES/TONGS'
#' \item 92 = 'POUNDS'
#' \item 93 = 'DRAG RAKE'
#' \item 95 = 'PUMPER'
#' \item 96 = 'HAND/HANDHELD TOOL'
#' \item 98 = 'FIXED GEAR'
#' }
#' @param showSp default is \code{FALSE} This tool can be used to narrow down fleets to those fleet
#' members who reported a particular species at any point in the specified time period.  If this is
#' set to T, then in addition to being able to select mdCode and gearCode from a select list, you
#' can also select from all of the reported landed species.
#' @param spCode default is \code{NULL} If this is set to a valid MARFIS species code, it will filter
#' the fleet members to only those who reported the selected species at some point in the specified
#' time frame.
#' @param quietly default is \code{FALSE}.  This indicates whether or not
#' information should be shown as the function proceeds.
#' @family fleets
#' @return returns a data.frame with 6 columns - "GEAR_CODE", "GEAR_DESC",
#'         "MD_CODE", "MD_DESC", "VR_NUMBER", "LICENCE_ID"
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_fleet<-function(fn.oracle.username = "_none_",
                    fn.oracle.password = "_none_",
                    fn.oracle.dsn = "_none_",
                    usepkg = "rodbc",
                    quietly = F,
                    dateStart = NULL, dateEnd = NULL,
                    mdCode = NULL, gearCode = NULL,
                    gearSpType = NULL, gearSpSize= NULL,
                    showSp = F, spCode = NULL){
  cxn = Mar.utils::make_oracle_cxn(usepkg,fn.oracle.username,fn.oracle.password,fn.oracle.dsn)
  mdCode=tolower(mdCode)
  gearCode=tolower(gearCode)
  spCode=as.numeric(spCode)
  keep<-new.env()
  keep$spDone <- keep$mdDone <- keep$gearDone <- keep$gearSpecsDone <- keep$canDoGearSpecs <- FALSE
  #if no end date, do it for 1 year
  if (is.null(dateEnd)) dateEnd = as.Date(dateStart,origin = "1970-01-01")+lubridate::years(1)

  #Narrow the data by only date range
  df = basicFleet(cxn, dateStart, dateEnd)
  # clean up the names of each md doc type
  bad = c("MONIT.*","DOCU.*","/ .*","FISHING .*","LOG.*"," FI$")
  for (b in 1:length(bad)){
    df$MD_DESC = sub(bad[b], "", df$MD_DESC)
  }
  df$MD_DESC <- trimws(df$MD_DESC)

  #Further narrow the data using md and gear - prompting if needed
  df = applyFilters(cxn = cxn, keep = keep, showSp = showSp, quietly = quietly, df = df, mdCode=mdCode, gearCode=gearCode, spCode = spCode,
                    gearSpType = gearSpType, gearSpSize = gearSpSize, dateStart = dateStart, dateEnd = dateEnd)
  # gearSpecPick <- getGearSpecs(df = df)
  if(nrow(df)<1) {
    cat("\n","No records found")
    return(NULL)
  }else{
    df <- df[with(df,order(VR_NUMBER, LICENCE_ID, MD_CODE, GEAR_CODE )),]
    return(df)
  }
}
