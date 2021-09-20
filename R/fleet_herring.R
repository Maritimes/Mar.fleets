#' @title fleet_herring
#' @description This is a wrapper function that facilitates extracting information for the herring fleet.
#' All of the information used to identify fleets is stored in the package's associated data files -
#' LIC_CORE, LIC_AREAS, and LIC_GEAR_SPEC.  The various wrappers can have different options (e.g.
#' MOBILE vs FIXED, WESTERN vs EASTERN, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square
#' mesh, etc), and depending on which options are selected, different fleets are identified, and
#' their data is extracted.
#' @param marfGear default is \code{c(24,31,41,42,61,63)}. This is a vector of MARFIS gear codes
#' known to have been used by this fleet. The default values can be replaced with a subset of these to
#' only return a gear-specific subset of the fleet's fishing activity.  If other values are provided,
#' the script will not run.
#' @param area default is \code{"4VWX5YP"}.  This restricts the data to particular NAFO areas.
#' Setting it to "ALL" or NULL removes the NAFO restrictions are returns all results.
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area -useLocal
#' @examples \dontrun{
#' db <- fleet_herring(useLocal = F,
#'                     year = 2018,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_herring(year = 2018,
#'                        useLocal = T,
#'                        data.dir = "c:/data_folder"
#'                       )
#'                        }
#' @family fleets
#' @inherit fleet_ return
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfSpp} = 200
#'   \item \code{isdbSpp} = 60
#'   \item \code{tripcd_id} = 60
#'   \item \code{fleet} = "HERRING"
#' }
#' The following parameters are "softcoded" - any or all of the values can be
#' provided, but other values are not allowed.
#' \itemize{
#'   \item \code{marfGear} = c(24,31,41,42,61,63)
#' }
#' @inherit fleet_ details
#' @export
fleet_herring <- function(marfGear = c(24,31,41,42,61,63), area= "4VWX5YP", useLocal = NULL, ...){
  isDraft()
  if (!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  valuesOK(valSent = marfGear, valID = "marfGear", valOK =   c(24,31,41,42,61,63))
  data = fleet_(fleet = "HERRING", marfSpp = 200, marfGear = marfGear, isdbSpp = 60, area = area, tripcd_id = 60, useLocal = useLocal,...)
  return(data)
}
