#' @title fleet_surfclam
#' @description This is a wrapper function that facilitates extracting information for the surfclam fleet.
#' All of the information used to identify fleets is stored in the package's associated data files -
#' LIC_CORE, LIC_AREAS, and LIC_GEAR_SPEC.  The various wrappers can have different options (e.g.
#' MOBILE vs FIXED, WESTERN vs EASTERN, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square
#' mesh, etc), and depending on which options are selected, different fleets are identified, and
#' their data is extracted.
#' @param areaFile  default is \code{"Areas_Surfclam_sf"}.  This is a spatial file with boundaries
#' appropriate for the fleet.  If NULL, set positions from MARFIS and ISDB data are compared to NAFO
#' divisions. Including \code{areaFile = "Areas_Surfclam_sf"} ensures that MARFIS and ISDB set
#' locations are compared to Surfclam-specific areas (i.e. strata),  but \code{NULL} or any dataset
#' from Mar.data is acceptable.
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area
#' @examples \dontrun{
#' db <- fleet_surfclam(year = 2018,
#'                     useLocal = F,
#'                     cxn  = <valid Oracle Connection>
#'                     )
#' local <- fleet_surfclam(year = 2018,
#'                        useLocal = T
#'                       )
#'                        }
#' @family fleets
#' @inherit fleet_ return
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfGear} = 71
#'   \item \code{marfSpp} = 608
#'   \item \code{isdbSpp} = 4355
#'   \item \code{tripcd_id} = 4355
#'   \item \code{fleet} = "SURFCLAM"
#' }
#' @inherit fleet_ details
#' @export
fleet_surfclam <- function(areaFile = "Areas_Surfclam_sf", useLocal = NULL, socks = FALSE, ...){
  if (!socks) isDraft()
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  data <- fleet_(fleet = "SURFCLAM", marfSpp = 608, marfGear = 71, isdbSpp = 4355, tripcd_id = 4355, areaFile = areaFile, useLocal = useLocal,...)
  return(data)
}
