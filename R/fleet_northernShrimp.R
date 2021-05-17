#' @title fleet_northernShrimp
#' @description This is a wrapper function that facilitates extracting information for the northern shrimp fleet.
#' All of the information used to identify fleets is stored in the package's associated data files - licCore, licAreas,
#' and licGearSpecs.  The various wrappers can have different options (e.g. mobile vs fixed, western
#' vs eastern, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square mesh, etc), and depending on which options are selected,
#' different fleets are identified, and their data is extracted.
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area -useLocal
#' @examples \dontrun{
#' db <- fleet_northernShrimp(useLocal = F,
#'                     year = 2018,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_northernShrimp(year = 2018,
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
fleet_northernShrimp <- function(useLocal = NULL, fleet=NULL,  ...){
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")

  fleet <-toupper(fleet)
  if (fleet %in% c("TRAP")){
    fleet = "NSHRIMPTRAP"
  }else if (fleet %in% c("TRAWL")){
    fleet = "NSHRIMPTRAWL"
  }else{
    stop("Specified type is invalid")
  }

  data = fleet_(fleet = fleet, marfSpp = c(702), isdbSpp = c(16,28), tripcd_id = c(2210), areaFile = "Areas_Shrimp_sf", useLocal = useLocal,...)
  return(data)
}
