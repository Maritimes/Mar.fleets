#' @title fleet_silverhake
#' @description This function is a wrapper function that facilitates extracting the following
#' information for the silver hake fleets:
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
#' @param mesh default is \code{'ALL'}. This indicates whether or not the the
#' fleet should be filtered by mesh size.  It can be either "ALL" or "SMALL"  (i.e. 55-65mm).
#' @param ... other arguments passed to methods
#' @examples \dontrun{
#' SilverHake <- fleet_silverhake(data.dir = "C:/myData")
#' }
#' @family fleets
#' @return list of objects, including marfis data, isdb data, information for matching isdb
#' and marfis data, and a summary of bycatch
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfSpp} = 172
#'   \item \code{nafoCode} = c('4V\%','4W\%','4X\%','5Y\%')
#'   \item \code{gearCode} = 12
#'   \item \code{mdCode} = 2
#' }
#'
#' For mesh = "SMALL"
#' \itemize{
#' \item \code{gearSpSize} =  seq(55,65,1)
#' }
#' @export
fleet_silverhake <- function(mesh = "all", ...){
  if (toupper(mesh) == "SMALL"){
    gearSpSize = seq(55,65,1)
  }
  marfSpp=172
  nafoCode=c('4V%','4W%','4X%','5Y%') #4VWX
  gearCode =12
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
