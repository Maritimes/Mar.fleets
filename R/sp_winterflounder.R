#' @title sp_winterflounder
#' @description This function is a wrapper function that facilitates extracting the following
#' information for the winter flounder fleet:
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
#' @param ... other arguments passed to methods
#' @examples \dontrun{
#' WinterFlounder <- sp_winterflounder(data.dir = "C:/myData")
#' }
#' @family species
#' @return list of objects, including marfis data, isdb data, information for matching isdb
#' and marfis data, and a summary of bycatch
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
sp_winterflounder <- function(...){

  # grab all parameters (sent and ...), replace ... val with specified
  args <- list(...)
  args[names(as.list(environment()))] <- as.list(environment())

  # Set up the winterflounder-specific variables -------------------------------------------------------
  marfSpp=143
  nafoCode=c('4X%','5Y%')
  gearCode = 12
  gearSpSize = seq(155,999,1)
  mdCode = 2

  argsFun <-  as.list(environment())
  argsFun[["args"]] <- NULL
  args[names(argsFun)] <- argsFun
  data <- do.call(get_all, args)
  return(data)
}
