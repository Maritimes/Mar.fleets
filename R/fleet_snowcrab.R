#' @title fleet_snowcrab
#' @description This is a wrapper function that facilitates extracting information for the snowcrab fleet.
#' All of the information used to identify fleets is stored in the package's associated data files - licCore, licAreas,
#' and licGearSpecs.  The various wrappers can have different options (e.g. mobile vs fixed, western
#' vs eastern, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square mesh, etc), and depending on which options are selected,
#' different fleets are identified, and their data is extracted.
#' @param area default is \code{'ALL'}. Other valid values are "SENS", "NENS", and "4X"
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area
#' @examples \dontrun{
#' db <- fleet_snowcrab(area = "UNIT2",
#'                     year = 2018,
#'                     useLocal = F,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_snowcrab(area = "UNIT2",
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
#'   \item \code{marfSpp} = 705
#'   \item \code{isdbSpp} = 2526
#'   \item \code{tripcd_id} = c(2509, 7061,7064)
#'   \item \code{fleet} = "SNOWCRAB"
#'   \item \code{areaFile} = "Areas_Snowcrab_sf"
#' }
#' @inherit fleet_ details
#' @export
fleet_snowcrab<- function(useLocal = NULL, ...){
  area <- toupper(area)

  message("This wrapper has never been QC'd")
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  data <- fleet_(fleet = "SNOWCRAB", marfSpp = 705, isdbSpp = 2526, tripcd_id = c(2509, 7061,7064), areaFile = "Areas_Snowcrab_sf", useLocal = useLocal,...)
  return(data)
}
