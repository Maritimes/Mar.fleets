#' @title fleet_halibut
#' @description This is a wrapper function that facilitates extracting information for the halibut fleet.
#' All of the information used to identify fleets is stored in the package's associated data files -
#' LIC_CORE, LIC_AREAS, and LIC_GEAR_SPEC.  The various wrappers can have different options (e.g.
#' MOBILE vs FIXED, WESTERN vs EASTERN, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square
#' mesh, etc), and depending on which options are selected, different fleets are identified, and
#' their data is extracted.
#' @param marfGear default is \code{c(12, 41, 51, 59)}. This is a vector of MARFIS gear codes
#' known to have been used by this fleet. The default values can be replaced with a subset of these to
#' only return a gear-specific subset of the fleet's fishing activity.  If other values are provided,
#' the script will not run.
#' @param area default is \code{"3NOPS4VWX5"}.  This restricts the data to particular NAFO areas.
#' Setting it to "ALL" or NULL removes the NAFO restrictions are returns all results.  For Halibut,
#' 2 other valid entries are \code{"3NOPS"} and \code{"4VWX5"}
#' @param areaFile  default is \code{"Areas_Halibut_sf"}.  This is a spatial file with boundaries
#' appropriate for the fleet.  If NULL, set positions from MARFIS and ISDB data are compared to NAFO
#' divisions.  Including \code{areaFile = "Areas_Halibut_sf"} ensures that MARFIS and ISDB set
#' locations are compared to Halibut-specific areas (i.e. strata),  but \code{NULL} or any dataset
#' from Mar.data is acceptable.
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area -useLocal
#' @examples \dontrun{
#' db <- fleet_halibut(useLocal = F,
#'                     year = 2018,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_halibut(year = 2018,
#'                        useLocal = T,
#'                        data.dir = "c:/data_folder"
#'                       )
#'                        }
#' @family fleets
#' @inherit fleet_ return
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfSpp} = 130
#'   \item \code{isdbSpp} = 30
#'   \item \code{tripcd_id} = c(30,7057,7058)
#'   \item \code{fleet} = "HALIBUT"
#' }
#' The following parameters are "softcoded" - any or all of the values can be
#' provided, but other values are not allowed.
#' \itemize{
#'   \item \code{marfGear} = c(12, 41, 51, 59)
#' }
#' @inherit fleet_ details
#' @export
fleet_halibut <- function(marfGear = c(12, 41, 51, 59), area= "3NOPS4VWX5", areaFile = 'Areas_Halibut_sf', useLocal = NULL, ...){
  isDraft()
  if (!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  if (is.null(area) || area =="ALL")area =="ALL"
  valuesOK(valSent = marfGear, valID = "marfGear", valOK =   c(12, 41, 51, 59))
  data = fleet_(fleet = "HALIBUT", marfSpp = 130, marfGear = marfGear, isdbSpp = 30, area = area, tripcd_id = c(30,7057,7058), areaFile = areaFile, useLocal = useLocal,...)
  return(data)
}
