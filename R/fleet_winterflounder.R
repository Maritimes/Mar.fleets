#' @title fleet_winterflounder
#' @description This is a wrapper function that facilitates extracting information for the winterflounder fleet.
#' All of the information used to identify fleets is stored in the package's associated data files - licCore, licAreas,
#' and licGearSpecs.  The various wrappers can have different options (e.g. mobile vs fixed, western
#' vs eastern, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square mesh, etc), and depending on which options are selected,
#' different fleets are identified, and their data is extracted.
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area
#' @examples \dontrun{
#' db <- fleet_winterflounder(year = 2018,
#'                     useLocal = F,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_winterflounder(year = 2018,
#'                        useLocal = T,
#'                        data.dir = "c:/data_folder"
#'                       )
#'                        }
#' @family fleets
#' @inherit fleet_ return
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfSpp} = 143
#'   \item \code{isdbSpp} = 43
#'   \item \code{tripcd_id} = 49
#'   \item \code{fleet} = "WINTERFLOUNDER"
#' }
#' @inherit fleet_ details
#' @export
fleet_winterflounder <- function(useLocal = NULL, ...){
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
   data <- fleet_(fleet = "WINTERFLOUNDER", marfSpp = 143, isdbSpp = 43, tripcd_id = c(49), useLocal = useLocal,...)
  return(data)
}
