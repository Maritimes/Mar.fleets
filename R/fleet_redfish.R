#' @title fleet_redfish
#' @description This function is a wrapper function that facilitates extracting the following
#' information for the redfish fleets:
#' \itemize{
#'   \item \code{fleet} - This is a dataframe of identifiers for all of the various trips undertaken by the
#'   selected fleet for the specified period (e.g. VRNs, licence IDs, Monitoring Document #s, etc)
#'   \item \code{marf} - This is a list of 3 sets of information for the commercial catch data (i.e. marfis)-
#'   the trips, the sets, and a special dataframe containing information that can be used to link
#'   the commercial data to the ISDB data
#'   \item \code{isdb} - This is a list of 4 data objects - 2 of which are all of the discovered ISDB data
#'   TRIPS and SETS for the fleet, as well as the TRIPS and SETS from the observer data that were
#'   sucessfully matched with the MARFIS data
#'   \item \code{bycatch} - This is a dataframe with the various species that were observed during observed
#'   trips.  For each species, the estimated number caught, the estimated kept wt (kgs) and the
#'   estimated discarded wt(kg) are all captured
#'   }
#' @param unit default is \code{NULL}. This is either "2" or "3".
#' @param ... other arguments passed to methods
#' @examples \dontrun{
#' Redfish <- fleet_redfish(unit = 2, data.dir = "C:/myData")
#'                       }
#' @family fleets
#' @return list of objects, including marfis data, isdb data, information for matching isdb
#' and marfis data, and a summary of bycatch
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfSpp} = 120
#'   \item \code{gearCode} = 12
#'   \item \code{mdCode} = 2
#' }
#' For unit = 2
#' \itemize{
#' \item \code{nafoCode} = c('4VS\%','4VN\%','4WF\%','4WG\%','4WJ\%','3PS\%')
#' \item \code{gearSpSize} = seq(90,115,1)
#' }
#' For unit = 3
#' \itemize{
#' \item \code{nafoCode} = c('4X\%','5YF\%','4WD\%','4WE\%','4WH\%','4WK\%','4WL\%')
#' \item \code{gearSpSize} = seq(110,115,1)
#' }
#' @export
fleet_redfish <- function(unit = NULL, ...){

  if (!is.null(unit)){
  if (unit==2){
    nafoCode= c('4VS%','4VN%','4WF%','4WG%','4WJ%','3PS%') #"4VSB" "4VSC" "4VSE" "4VSU" "4VSV" - add others to remove U
    gearSpSize = seq(90,115,1)
  } else if (unit==3){
    nafoCode= c('4X%','5YF%','4WD%','4WE%','4WH%','4WK%','4WL%')
    gearSpSize = seq(110,115,1)
  }
}
  marfSpp=120
  gearCode = 12
  mdCode = 2

  argsFn <- as.list(environment())
  argsUser <- list(...)

  if((length(argsUser$debug)>0) && (argsUser$debug == TRUE)) {
    Mar.utils::where_now(inf = as.character(sys.calls()[[sys.nframe()]]))
    startTime=Sys.time()
  }

  data <- do.call(get_all, list(argsFn= argsFn, argsUser=argsUser))
  if(exists("startTime")) cat("\n","Completed in",round( difftime(Sys.time(),startTime,units = "secs"),0),"secs\n")
  return(data)
}
