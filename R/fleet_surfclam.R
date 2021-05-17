#' @title fleet_surfclam
#' @description This is a wrapper function that facilitates extracting information for the surfclam fleet.
#' All of the information used to identify fleets is stored in the package's associated data files - licCore, licAreas,
#' and licGearSpecs.  The various wrappers can have different options (e.g. mobile vs fixed, western
#' vs eastern, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square mesh, etc), and depending on which options are selected,
#' different fleets are identified, and their data is extracted.
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area
#' @examples \dontrun{
#' db <- fleet_surfclam(year = 2018,
#'                     useLocal = F,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_surfclam(year = 2018,
#'                        useLocal = T,
#'                        data.dir = "c:/data_folder"
#'                       )
#'                        }
#' @family fleets
#' @inherit fleet_ return
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfSpp} = 608
#'   \item \code{isdbSpp} = 4355
#'   \item \code{tripcd_id} = 4355
#'   \item \code{fleet} = "SURFCLAM"
#'   \item \code{areaFile} = "Areas_Surfclam_sf"
#' }
#' @inherit fleet_ details
#' @export
fleet_surfclam <- function(useLocal = NULL, ...){
  message("This wrapper has never been QC'd")
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  data <- fleet_(fleet = "SURFCLAM", marfSpp = 608, isdbSpp = 4355, tripcd_id = c(4355), areaFile = "Areas_Surfclam_sf", useLocal = useLocal,...)
  return(data)
}
