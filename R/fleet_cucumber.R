#' @title fleet_cucumber
#' @description This is a wrapper function that facilitates extracting information for the sea cucumber fleet.
#' All of the information used to identify fleets is stored in the package's associated data files - licCore, licAreas,
#' and licGearSpecs.  The various wrappers can have different options (e.g. mobile vs fixed, western
#' vs eastern, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square mesh, etc), and depending on which options are selected,
#' different fleets are identified, and their data is extracted.
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area -useLocal
#' @examples \dontrun{
#' db <- fleet_cucumber(useLocal = F,
#'                     year = 2018,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_cucumber(year = 2018,
#'                        useLocal = T,
#'                        data.dir = "c:/data_folder"
#'                       )
#'                        }
#' @family fleets
#' @inherit fleet_ return
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfSpp} = c(619)
#'   \item \code{isdbSpp} = c(6600,6611)
#'   \item \code{tripcd_id} = c(6600)
#'   \item \code{fleet} = "CUCUMBER"
#' }
#' @inherit fleet_ details
#' @export
fleet_cucumber <- function(useLocal = NULL, ...){
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  # stop("wrapper in progress")
  data = fleet_(fleet = "CUCUMBER", marfSpp = c(619), isdbSpp = c(6600,6611), tripcd_id = c(6600), useLocal = useLocal,...)
  return(data)
}
