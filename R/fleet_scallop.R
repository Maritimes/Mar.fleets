#' @title fleet_scallop
#' @description This is a wrapper function that facilitates extracting information for the scallop fleet.
#' All of the information used to identify fleets is stored in the package's associated data files -
#' LIC_CORE, LIC_AREAS, and LIC_GEAR_SPEC.  The various wrappers can have different options (e.g.
#' MOBILE vs FIXED, WESTERN vs EASTERN, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square
#' mesh, etc), and depending on which options are selected, different fleets are identified, and
#' their data is extracted.
#' @param fleet default is NULL.  Valid values are "INSHORE" or "OFFSHORE"
#' @param areaFile  default is \code{NULL}.  This is a spatial file with boundaries appropriate for
#' the fleet.  If NULL, set positions from MARFIS and ISDB data are compared to NAFO divisions.
#' Normal, valid values for this wrapper include "NULL", "Areas_Scallop_sf", and "SPAs_Scallop_sf",
#' but any dataset from Mar.data is acceptable.
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
#'   \item \code{marfGear} = 71
#'   \item \code{marfSpp} = 612
#'   \item \code{isdbSpp} = 4321
#'   \item \code{tripcd_id} = c(4320,7062)
#'   \item \code{fleet} = "SCALLOP_OFF" or "SCALLOP_INSH", depending on selections
#' }
#' @inherit fleet_ details
#' @export
fleet_scallop <- function(fleet = NULL, areaFile = NULL, useLocal = NULL, socks = FALSE, ...){
  if (!socks) isDraft()
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  fleet <- toupper(fleet)
  if (fleet == "OFFSHORE"){
    fleet <- "SCALLOP_OFF"
    if (is.null(areaFile)) areaFile = "Areas_Scallop_sf"
  }else if (fleet == "INSHORE"){
    fleet <- "SCALLOP_INSH"
    if (is.null(areaFile)) areaFile = "SPAs_Scallop_sf"
  }else if (fleet == "UNKNOWN"){
    fleet <- "SCALLOP_UNK"
    if (is.null(areaFile)) areaFile = "Areas_Scallop_sf"
  }else{
    stop("Value for 'fleet' is unrecognized")
  }
  data <- fleet_(fleet = fleet, marfSpp = 612, marfGear = 71, isdbSpp = 4321, tripcd_id = c(4320,7062), areaFile = areaFile, useLocal = useLocal,...)
  return(data)
}
