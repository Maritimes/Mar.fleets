#' @title fleet_sculpin
#' @description This is a wrapper function that facilitates extracting information for the sculpin fleet.
#' All of the information used to identify fleets is stored in the package's associated data files -
#' LIC_CORE, LIC_AREAS, and LIC_GEAR_SPEC.  The various wrappers can have different options (e.g.
#' MOBILE vs FIXED, WESTERN vs EASTERN, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square
#' mesh, etc), and depending on which options are selected, different fleets are identified, and
#' their data is extracted.
#' @param marfGear default is \code{c(12,41,51,59)}. This is a vector of MARFIS gear codes
#' known to have been used by this fleet. The default values can be replaced with a subset of these to
#' only return a gear-specific subset of the fleet's fishing activity.  If other values are provided,
#' the script will not run.
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area -useLocal
#' @examples \dontrun{
#' db <- fleet_sculpin(useLocal = F,
#'                     year = 2018,
#'                     cxn  = <valid Oracle Connection>
#'                     )
#' local <- fleet_sculpin(year = 2018,
#'                        useLocal = T
#'                       )
#'                        }
#' @family fleets
#' @inherit fleet_ return
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfSpp} = 181
#'   \item \code{isdbSpp} = 300
#'   \item \code{fleet} = "SCULPIN"
#' }
#' The following parameters are "softcoded" - any or all of the values can be
#' provided, but other values are not allowed.
#' \itemize{
#'   \item \code{marfGear} = c(12,41,51,59)
#' }
#' @inherit fleet_ details
#' @export
fleet_sculpin <- function(marfGear = c(12,41,51,59), useLocal = NULL, socks = FALSE, ...){
  if (!socks) isDraft()
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  valuesOK(valSent = marfGear, valID = "marfGear", valOK =   c(12,41,51,59))
  data = fleet_(fleet = "SCULPIN", marfSpp = 181, marfGear = marfGear, gearSpecs = "ALL", isdbSpp = 300,  useLocal = useLocal,...)
  return(data)
}
