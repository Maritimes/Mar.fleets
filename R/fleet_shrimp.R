#' @title fleet_shrimp
#' @description This is a wrapper function that facilitates extracting information for the shrimp fleet.
#' All of the information used to identify fleets is stored in the package's associated data files -
#' LIC_CORE, LIC_AREAS, and LIC_GEAR_SPEC.  The various wrappers can have different options (e.g.
#' MOBILE vs FIXED, WESTERN vs EASTERN, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square
#' mesh, etc), and depending on which options are selected, different fleets are identified, and
#' their data is extracted.
#' @param marfGear default is \code{c(19,62)}. This is a vector of MARFIS gear codes known to have caught
#' this species. The default values can be replaced with a smaller selection to only return information
#' for a gear-specific subset of fishing activity.
#' @param type default is \code{NULL}. This is either "TRAWL" or "TRAP".
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
#'   \item \code{marfSpp} = c(702)
#'   \item \code{isdbSpp} = c(16,28)
#'   \item \code{tripcd_id} = c(2210)
#'   \item \code{fleet} = "NSHRIMP"
#'   \item \code{areaFile} = Areas_Shrimp_sf
#' }
#' @inherit fleet_ details
#' @export
fleet_shrimp <- function(marfGear = c(19,62), type=NULL,  useLocal = NULL, ...){
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")

  if (!is.null(type) && type =="TRAP"){
    marfGear = c(62)
  }else if (!is.null(type) && type == "TRAWL"){
    marfGear = c(19)
  }

  data = fleet_(fleet = "SHRIMP", marfSpp = c(702), marfGear = marfGear, isdbSpp = c(16,28), tripcd_id = c(2210), areaFile = "Areas_Shrimp_sf", useLocal = useLocal,...)
  return(data)
}
