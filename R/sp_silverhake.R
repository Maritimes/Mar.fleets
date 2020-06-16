#' @title sp_silverhake
#' @description This function is a wrapper function that facilitates extracting the following
#' information for the silver hake fleets:
#' \itemize{
#'   \item \code{fleet} - This is a dataframe of identifiers for all of the various trips undertaken by the
#'   selected fleet for the specified period (e.g. VRNs, licence IDs, Monitoring Document #s, etc)
#'   \item \code{marf} - This is a list of 3 sets of information for the commercial catch data (i.e. marfis)-
#'   the trips, the sets, and a special dataframe containing information that can be used to link
#'   the commercial data to the observer data
#'   \item \code{obs} - This is a list of 4 data objects - 2 of which are all of the discovered observer data
#'   TRIPS and SETS for the fleet, as well as the TRIPS and SETS from teh observer data that were
#'   sucessfully matched with the MARFIS data
#'   \item \code{bycatch} - This is a dataframe with the various species that were observed during observed
#'   trips.  For each species, the estimated number caught, the estimated kept wt (kgs) and the
#'   estimated discarded wt(kg) are all captured
#' }
#' @param useLocal default is \code{FALSE}. By default, these scripts query Oracle.  If you want to
#' run them against local copies of the data, please set to TRUE.
#' @param year default is \code{NULL}. This is a year (YYYY) for which you want to look at the marfis,
#' observer and bycatch data.
#' @examples \dontrun{
#' SilverHake <- sp_silverhake(year = 2018, data.dir = "C:/myData")
#' }
#' @family species
#' @return list of objects, including marfis data, observer data, information for matching observer
#' and marfis data, and a summary of bycatch
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
sp_silverhake <- function(year=NULL, ...){

  # Set up the silver hake-specific variables -------------------------------------------------------
  args <- list(marfSpp=172,
               nafoCode=c('4V%','4W%','4X%'),#4VWX
               gearCode =12,
               mdCode = 2,
               dateStart =paste0(year,"-01-01"),
               dateEnd =paste0(year,"-12-31")
  )


  argsSent <- as.list(match.call(expand.dots=TRUE))[-1]
  args[names(argsSent)] <- argsSent

  data <- do.call(get_all, args)
  return(data)
}
