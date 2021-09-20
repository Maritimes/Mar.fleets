#' @title fleet_snowcrab
#' @description This is a wrapper function that facilitates extracting information for the snowcrab fleet.
#' All of the information used to identify fleets is stored in the package's associated data files -
#' LIC_CORE, LIC_AREAS, and LIC_GEAR_SPEC.  The various wrappers can have different options (e.g.
#' MOBILE vs FIXED, WESTERN vs EASTERN, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square
#' mesh, etc), and depending on which options are selected, different fleets are identified, and
#' their data is extracted.
#' @param areaFile  default is \code{"Areas_Snowcrab_sf"}.  This is a spatial file with boundaries
#' appropriate for the fleet.  If NULL, set positions from MARFIS and ISDB data are compared to NAFO
#' divisions. Including \code{areaFile = "Areas_Snowcrab_sf"} ensures that MARFIS and ISDB set
#' locations are compared to Snowcrab-specific areas (i.e. strata),  but \code{NULL} or any dataset
#' from Mar.data is acceptable.
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area
#' @examples \dontrun{
#' db <- fleet_snowcrab(year = 2018,
#'                     useLocal = F,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_snowcrab(year = 2018,
#'                        useLocal = T,
#'                        data.dir = "c:/data_folder"
#'                       )
#'                        }
#' @family fleets
#' @inherit fleet_ return
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfGear} = 62
#'   \item \code{marfSpp} = 705
#'   \item \code{isdbSpp} = 2526
#'   \item \code{tripcd_id} = c(2509, 7061,7064)
#'   \item \code{fleet} = "SNOWCRAB"
#' }
#' @inherit fleet_ details
#' @export
fleet_snowcrab<- function(areaFile = "Areas_Snowcrab_sf", useLocal = NULL, ...){
  isDraft()
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  data <- fleet_(fleet = "SNOWCRAB", marfSpp = 705, marfGear = 62, isdbSpp = 2526, tripcd_id = c(2509, 7061,7064), areaFile = areaFile, useLocal = useLocal,...)
  return(data)
}
