#' @title fleet_shrimp
#' @description This is a wrapper function that facilitates extracting information for the shrimp fleet.
#' All of the information used to identify fleets is stored in the package's associated data files -
#' LIC_CORE, LIC_AREAS, and LIC_GEAR_SPEC.  The various wrappers can have different options (e.g.
#' MOBILE vs FIXED, WESTERN vs EASTERN, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square
#' mesh, etc), and depending on which options are selected, different fleets are identified, and
#' their data is extracted.
#' @param marfGear default is \code{c(19,62)}. This is a vector of MARFIS gear codes
#' known to have been used by this fleet. The default values can be replaced with a subset of these to
#' only return a gear-specific subset of the fleet's fishing activity.  If other values are provided,
#' the script will not run.
#' @param areaFile  default is \code{"Areas_Shrimp_sf"}.  This is a spatial file with boundaries
#' appropriate for the fleet.  If NULL, set positions from MARFIS and ISDB data are compared to NAFO
#' divisions. Including \code{areaFile = "Areas_Shrimp_sf"} ensures that MARFIS and ISDB set
#' locations are compared to Shrimp-specific areas (i.e. strata),  but \code{NULL} or any dataset
#' from Mar.data is acceptable.
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area -useLocal
#' @examples \dontrun{
#' db <- fleet_shrimp(useLocal = F,
#'                     year = 2018,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_shrimp(year = 2018,
#'                        useLocal = T,
#'                        data.dir = "c:/data_folder"
#'                       )
#'                        }
#' @family fleets
#' @inherit fleet_ return
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfSpp} = 702
#'   \item \code{isdbSpp} = c(16,28)
#'   \item \code{tripcd_id} = 2210
#'   \item \code{fleet} = "NSHRIMP"
#' }
#' The following parameters are "softcoded" - any or all of the values can be
#' provided, but other values are not allowed.
#' \itemize{
#'   \item \code{marfGear} = c(19,62)
#' }
#' @inherit fleet_ details
#' @export
fleet_shrimp <- function(marfGear = c(19,62),  areaFile = "Areas_Shrimp_sf", useLocal = NULL, socks = FALSE, ...){
  if (!socks) isDraft()
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  valuesOK(valSent = marfGear, valID = "marfGear", valOK =   c(19,62))
  data = fleet_(fleet = "SHRIMP", marfSpp = 702, marfGear = marfGear, isdbSpp = c(16,28), tripcd_id = 2210, areaFile = areaFile, useLocal = useLocal,...)
  return(data)
}
