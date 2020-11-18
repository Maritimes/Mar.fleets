#' @title fleet_pollock
#' @description This function is a wrapper function that facilitates extracting the following
#' information for the pollock fleets:
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
#'   estimated discarded wt(kg) are all captured}
#' @param type default is \code{NULL}. This is either "FIXED" or "MOBILE".
#' @param mesh default is \code{'ALL'}. This is either "SMALL" (i.e. 1-129mm) or "LARGE" (i.e. 130mm+), or "ALL".
#' @param component default is \code{NULL}. This is either "WESTERN" or "EASTERN".
#' @param ... other arguments passed to methods
#' @examples \dontrun{
#' Pollock_West_m_sm <- fleet_pollock(type = "MOBILE",
#'                                 mesh="SMALL", component = "WESTERN",
#'                                 data.dir = "C:/myData")
#'                                 }
#' \dontrun{
#' Pollock_East_m_lg <- fleet_pollock(type = "MOBILE",
#'                                 mesh="LARGE", component = "EASTERN",
#'                                 data.dir = "C:/myData")
#'                                 }
#' @family fleets
#' @return list of objects, including marfis data, isdb data, information for matching isdb
#' and marfis data, and a summary of bycatch
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfSpp} =170
#' }
#' For component = "WESTERN"
#' \itemize{
#' \item \code{nafoCode} = c('4XO\%','4XP\%','4XQ\%','4XR\%','4XS\%','5\%') ("MOBILE")
#' \item \code{nafoCode} = c('4XO\%','4XP\%','4XQ\%','4XR\%','4XS\%','5\%', '4XU\%') ("FIXED")
#' \item \code{gearSpSize} = seq(90,115,1)
#' }
#' For component = "EASTERN"
#' \itemize{
#' \item \code{nafoCode} c('4XM\%','4XN\%','4V\%','4W\%')
#' }
#' For type = "MOBILE"
#' \itemize{
#' \item \code{mdCode} = c(2)
#' \item \code{gearCode} = c(12)
#' \item \code{gearSpSize} = seq(1,129,1) ("SMALL")
#' \item \code{gearSpSize} = seq(130,999,1) ("LARGE")
#' }
#' For type = "FIXED"
#' \itemize{
#' \item \code{mdCode} = c(1, 29)
#' \item \code{gearCode} = c(40,41)
#' \item \code{gearSpSize} = 'all'

#' }
#' @export
fleet_pollock <- function(type = NULL, mesh='ALL', component = NULL, ...){

  # Set up the Pollock-specific variables -------------------------------------------------------
  if (toupper(component)=="WESTERN"){
    nafoCode= c('4XO%','4XP%','4XQ%','4XR%','4XS%','5%') #'4XU%' intentionally excluded as directed by HS
    if (toupper(type) == "FIXED") nafoCode <-c(nafoCode, '4XU%')
  } else {
    nafoCode= c('4XM%','4XN%','4V%','4W%')
  }

  if (toupper(type) == "MOBILE"){
    mdCode = c(2)
    gearCode = c(12)
    #gearSpSize = seq(130,999,1)#lgmesh
    if (toupper(mesh)=="SMALL") {
      gearSpSize = seq(1,129,1)#smmesh
    }else if (toupper(mesh)=="LARGE"){
      gearSpSize = seq(130,999,1)
    }else{
      gearSpSize ="all"
    }
  }else if (toupper(type) == "FIXED"){
    mdCode = c(1, 29)
    gearCode = c(40,41)
    gearSpSize = 'all'
  }

  marfSpp=170

  argsFn <- as.list(environment())
  argsUser <- list(...)

  if((length(argsUser$debug)>0) && (argsUser$debug == TRUE)) Mar.utils::where_now(inf = as.character(sys.calls()[[sys.nframe()]]))

  data <- do.call(get_all, list(argsFn= argsFn, argsUser=argsUser))
  return(data)
}
