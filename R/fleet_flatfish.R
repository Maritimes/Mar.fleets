#' @title fleet_flatfish
#' @description This is a wrapper function that facilitates extracting information for the flatfish fleet.
#' All of the information used to identify fleets is stored in the package's associated data files -
#' LIC_CORE, LIC_AREAS, and LIC_GEAR_SPEC.  The various wrappers can have different options (e.g.
#' MOBILE vs FIXED, WESTERN vs EASTERN, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square
#' mesh, etc), and depending on which options are selected, different fleets are identified, and
#' their data is extracted.
#' @param marfGear default is \code{c(12,41,51,59)}. This is a vector of MARFIS gear codes
#' known to have been used by this fleet. The default values can be replaced with a subset of these to
#' only return a gear-specific subset of the fleet's fishing activity.  If other values are provided,
#' the script will not run.
#' @param area default is \code{NULL}.  Valid values are "4VW" and "4VWX".
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area
#' @examples \dontrun{
#' db <- fleet_flatfish(year = 2018,
#'                     area = "4VW",
#'                     useLocal = F,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_flatfish(year = 2018,
#'                        area = "4VWX",
#'                        useLocal = T,
#'                        data.dir = "c:/data_folder"
#'                       )
#'                        }
#' @family fleets
#' @inherit fleet_ return
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfSpp} = c(140,141,142,143,144,145,146,149)
#'   \item \code{isdbSpp} = c(40,41,42,43,44,45,49,140,141,142,143,196,198,282,344,345,385,386,346)
#'   \item \code{tripcd_id} = 49           FLATFISH
#'   \item \code{fleet} = "FLATFISH"
#' }
#' The following parameters are "softcoded" - any or all of the values can be
#' provided, but other values are not allowed.
#' \itemize{
#'   \item \code{marfGear} = c(12,21,22)
#' }
#' @inherit fleet_ details
#' @export
fleet_flatfish <- function(marfGear = c(10,12,15,21,41,51,53,58,59,60,81,98), area= NULL, useLocal = NULL, socks = FALSE, ...){
  if (is.null(area)) stop("'area' cannot be NULL")
  area<- toupper(area)
  if (!socks) isDraft()
  # if (area=="4VW"){
  #   marfGear = c(12)
  # }else if (area=="4VWX"){
  #   marfGear <- c(21,22)
  # }

  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  # valuesOK(valSent = marfGear, valID = "marfGear", valOK =   c(12,21,22))
  data <- fleet_(fleet = "FLATFISH", marfSpp = c(140,141,142,143,144,145,146,149), marfGear = marfGear, gearSpecs = area, isdbSpp = c( 40, 41, 42, 43, 44, 45, 49, 140, 141, 142, 143, 196, 198, 282, 344, 345, 385, 386, 346), tripcd_id = 49, useLocal = useLocal,...)
  return(data)
}
