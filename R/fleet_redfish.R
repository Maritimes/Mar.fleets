#' @title fleet_redfish
#' @description This is a wrapper function that facilitates extracting information for the redfish fleet.
#' All of the information used to identify fleets is stored in the package's associated data files -
#' LIC_CORE, LIC_AREAS, and LIC_GEAR_SPEC.  The various wrappers can have different options (e.g.
#' MOBILE vs FIXED, WESTERN vs EASTERN, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square
#' mesh, etc), and depending on which options are selected, different fleets are identified, and
#' their data is extracted.
#' @param marfGear default is \code{c(12,21,41,51,59)}. This is a vector of MARFIS gear codes known to have caught
#' this species. The default values can be replaced with a smaller selection to only return information
#' for a gear-specific subset of fishing activity.
#' @param unit default is \code{NULL}.  Valid selections include \code{"UNIT2"} and \code{"UNIT3"}
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area
#' @examples \dontrun{
#' db <- fleet_redfish(unit = "UNIT2",
#'                     useLocal = F,
#'                     year = 2018,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_redfish(unit = "UNIT2",
#'                        useLocal = T,
#'                        year = 2018,
#'                        data.dir = "c:/data_folder"
#'                       )
#'                        }
#' @family fleets
#' @inherit fleet_ return
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfSpp} = 120
#'   \item \code{isdbSpp} = 23
#'   \item \code{tripcd_id} = 23
#'   \item \code{fleet} = "REDFISH"
#'   \item \code{area} = "UNIT2" or "UNIT3", depending on selections
#'   \item \code{gearSpecs} = "UNIT2" or "UNIT3", depending on selections
#' }
#' @inherit fleet_ details
#' @export
fleet_redfish <- function(marfGear = c(12,21,41,51,59), unit = NULL, useLocal = NULL, ...){
  unit = toupper(unit)
  if (unit == "UNIT2" | unit == "2" | unit == 2){
    area <- "UNIT2"
    gearSpecs <- "UNIT2"
  } else if (unit == "UNIT3" | unit == "3" | unit == 3){
    area = "UNIT3"
    gearSpecs =  "UNIT3"
  } else{
    stop("Unknown UNIT")
  }

  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")

  data = fleet_(fleet = "REDFISH", marfSpp = 120, marfGear = marfGear, isdbSpp = 23, area = area, gearSpecs = gearSpecs, tripcd_id = c(23), useLocal = useLocal,...)
  return(data)
}
