#' @title fleet_scallop
#' @description This is a wrapper function that facilitates extracting information for the scallop fleet.
#' All of the information used to identify fleets is stored in the package's associated data files - licCore, licAreas,
#' and licGearSpecs.  The various wrappers can have different options (e.g. mobile vs fixed, western
#' vs eastern, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square mesh, etc), and depending on which options are selected,
#' different fleets are identified, and their data is extracted.
#' @param fleet default is NULL.  Valid values are "INSHORE" or "OFFSHORE"
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area
#' @examples \dontrun{
#' db <- fleet_scallop(fleet = "INSHORE",
#'                     year = 2018,
#'                     useLocal = F,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_scallop(fleet = "OFFSHORE",
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
#'   \item \code{marfSpp} = 612
#'   \item \code{isdbSpp} = 4321
#'   \item \code{tripcd_id} = c(4320,7062)
#'   \item \code{fleet} = "SCALLOP_OFF" or "SCALLOP_INSH", depending on selections
#'   \item \code{areaFile} == "Areas_Scallop_sf"
#' }
#' @inherit fleet_ details
#' @export
fleet_scallop <- function(fleet = NULL, useLocal = NULL, ...){
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  fleet <- toupper(fleet)
  fleet <- ifelse(fleet == "OFFSHORE", "SCALLOP_OFF", "SCALLOP_INSH")
  data <- fleet_(fleet = fleet, marfSpp = 612, isdbSpp = 4321, tripcd_id = c(4320,7062), areaFile = "Areas_Scallop_sf", useLocal = useLocal,...)
  return(data)
}
