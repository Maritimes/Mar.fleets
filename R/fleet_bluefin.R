#' @title fleet_bluefin
#' @description This is a wrapper function that facilitates extracting information for the Bluefin tuna fleet.
#' All of the information used to identify fleets is stored in the package's associated data files -
#' LIC_CORE, LIC_AREAS, and LIC_GEAR_SPEC.  The various wrappers can have different options (e.g.
#' MOBILE vs FIXED, WESTERN vs EASTERN, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square
#' mesh, etc), and depending on which options are selected, different fleets are identified, and
#' their data is extracted.
#' @param marfGear default is \code{c(54,60)}. This is a vector of MARFIS gear codes
#' known to have been used by this fleet. The default values can be replaced with a subset of these to
#' only return a gear-specific subset of the fleet's fishing activity.  If other values are provided,
#' the script will not run.
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area -useLocal
#' @examples \dontrun{
#' db <- fleet_bluefin(useLocal = F,
#'                     year = 2018,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_bluefin(year = 2018,
#'                        useLocal = T,
#'                        data.dir = "c:/data_folder"
#'                       )
#'                        }
#' @family fleets
#' @inherit fleet_ return
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfSpp} = c(254)
#'   \item \code{isdbSpp} = c(71)
#'   \item \code{tripcd_id} = c(73,7059)
#'   \item \code{fleet} = "BLUEFIN"
#' }
#' @inherit fleet_ details
#' @export
fleet_bluefin <- function(marfGear = c(54,60), useLocal = NULL, socks = FALSE, ...){
  if (!socks) isDraft()
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  if (any(!(marfGear) %in% c(54,60))) stop("Please limit specified values of 'marfGear' to any/all of the default values.")
  # stop("wrapper in progress")
    data = fleet_(fleet = "BLUEFIN", marfSpp = c(254), marfGear = marfGear, isdbSpp = c(71), tripcd_id = c(73,7059), useLocal = useLocal,...)
  return(data)
}
