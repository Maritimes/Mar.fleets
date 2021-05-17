#' @title fleet_haddock
#' @description This is a wrapper function that facilitates extracting information for the haddock fleets.
#' All of the information used to identify fleets is stored in the package's associated data files - licCore, licAreas,
#' and licGearSpecs.  The various wrappers can have different options (e.g. mobile vs fixed, western
#' vs eastern, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square mesh, etc), and depending on which options are selected,
#' different fleets are identified, and their data is extracted.
#' @param type default is \code{NULL}. This is either "FIXED" or "MOBILE".
#' @param area default is \code{NULL}. This is either "4X5Y" or "5ZJM".
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area
#' @examples \dontrun{
#' db <- fleet_haddock(type = "FIXED",
#'                     area = "4X5Y",
#'                     year = 2018,
#'                     useLocal = F,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_haddock(type = "MOBILE",
#'                        area = "5ZJM",
#'                        year = 2018,
#'                        useLocal = T,
#'                        data.dir = "c:/data_folder"
#'                       )
#'                        }
#' @family fleets
#' @inherit fleet_ return
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfSpp} = 110
#'   \item \code{isdbSpp} = 11
#'   \item \code{tripcd_id} = 7001
#'   \item \code{fleet} = HADDOCK_MOB or HADDOCK_FIXED, depending on selections
#'   \item \code{area} = 4X5Y, 5ZJM, or all depending on selections
#'   \item \code{gearSpecs} = 4X5Y or ALL, depending on selections
#'
#' }
#' @inherit fleet_ details
#' @export
fleet_haddock <- function(type = NULL, area= NULL, useLocal = NULL, ...){
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  type <- toupper(type)
  area <- toupper(area)

  fleet <- ifelse(type == "MOBILE", "HADDOCK_MOB", "HADDOCK_FIXED")
  gearSpecs <- ifelse(type == "MOBILE", "4X5Y", "ALL")
  area <- ifelse(type == "MOBILE", ifelse(area == "4X5Y", "4X5Y", "5ZJM"), "ALL")
  data <- fleet_(fleet = fleet, marfSpp = 110, isdbSpp = 11, area = area, gearSpecs = gearSpecs, tripcd_id = c(7001), useLocal = useLocal,...)
  return(data)
}
