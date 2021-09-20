#' @title fleet_redCrab
#' @description This is a wrapper function that facilitates extracting information for the red crab fleet.
#' All of the information used to identify fleets is stored in the package's associated data files -
#' LIC_CORE, LIC_AREAS, and LIC_GEAR_SPEC.  The various wrappers can have different options (e.g.
#' MOBILE vs FIXED, WESTERN vs EASTERN, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square
#' mesh, etc), and depending on which options are selected, different fleets are identified, and
#' their data is extracted.
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area -useLocal
#' @examples \dontrun{
#' db <- fleet_redCrab(useLocal = F,
#'                     year = 2018,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_redCrab(year = 2018,
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
#'   \item \code{marfSpp} = c(706,742)
#'   \item \code{isdbSpp} = 2532
#'   \item \code{tripcd_id} = 2509
#'   \item \code{fleet} = "REDCRAB"
#' }
#' @inherit fleet_ details
#' @export
fleet_redCrab <- function(useLocal = NULL, ...){
  isDraft()
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  data = fleet_(fleet = "REDCRAB", marfSpp = c(706,742), marfGear = 62, isdbSpp = 2532, tripcd_id = 2509, useLocal = useLocal,...)
  return(data)
}
