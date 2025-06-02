#' @title fleet_rockCrab
#' @description This is a wrapper function that facilitates extracting information for the rock crab fleet.
#' All of the information used to identify fleets is stored in the package's associated data files -
#' LIC_CORE, LIC_AREAS, and LIC_GEAR_SPEC.  The various wrappers can have different options (e.g.
#' MOBILE vs FIXED, WESTERN vs EASTERN, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square
#' mesh, etc), and depending on which options are selected, different fleets are identified, and
#' their data is extracted.
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area -useLocal
#' @examples \dontrun{
#' db <- fleet_rockCrab(useLocal = F,
#'                     year = 2018,
#'                     cxn  = <valid Oracle Connection>
#'                     )
#' local <- fleet_rockCrab(year = 2018,
#'                        useLocal = T
#'                       )
#'                        }
#' @family fleets
#' @inherit fleet_ return
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfGear} = 62
#'   \item \code{marfSpp} = c(704,742)
#'   \item \code{isdbSpp} = 2513
#'   \item \code{tripcd_id} = 2509
#'   \item \code{fleet} = "ROCKCRAB"
#' }
#' @inherit fleet_ details
#' @export
fleet_rockCrab <- function(useLocal = NULL, socks = FALSE, ...){
  if (!socks) isDraft()
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  data = fleet_(fleet = "ROCKCRAB", marfSpp = c(704,742), marfGear = 62, isdbSpp = 2513,tripcd_id = 2509, useLocal = useLocal,...)
  return(data)
}
