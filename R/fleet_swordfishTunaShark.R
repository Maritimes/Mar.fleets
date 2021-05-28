#' @title fleet_swordfishTunaShark
#' @description This is a wrapper function that facilitates extracting information for the large
#' pelagics fleet (i.e. swordfish, tuna, shark).
#' All of the information used to identify fleets is stored in the package's associated data files -
#' LIC_CORE, LIC_AREAS, and LIC_GEAR_SPEC.  The various wrappers can have different options (e.g.
#' MOBILE vs FIXED, WESTERN vs EASTERN, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square
#' mesh, etc), and depending on which options are selected, different fleets are identified, and
#' their data is extracted.
#' @param marfGear default is \code{''}. This is a vector of MARFIS gear codes known to have caught
#' this species. The default values can be replaced with a smaller selection to only return information
#' for a gear-specific subset of fishing activity.
#' @param type default is \code{NULL}. This is either "LL" (longline), "HARPOON", or "OTHER" (e.g. tended line. angling).
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area -useLocal
#' @examples \dontrun{
#' db <- fleet_swordfishTunaShark(useLocal = F,
#'                     year = 2018,
#'                     type= "HARPOON",
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_swordfishTunaShark(year = 2018,
#'                        type = "LL",
#'                        useLocal = T,
#'                        data.dir = "c:/data_folder"
#'                       )
#'                        }
#' @family fleets
#' @inherit fleet_ return
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfSpp} = c(251,257,259,379)
#'   \item \code{isdbSpp} = c(72, 73, 190, 191, 192, 846,215,223,230,231,233,234,237,246,592,965,1004)
#'   \item \code{tripcd_id} = c(72,73)
#'   \item \code{fleet} = SWORDFISHTUNASLL or SWORDFISHTUNASHARP, depending on value for type
#' }
#' @inherit fleet_ details
#' @export
fleet_swordfishTunaShark <- function(marfGear = c(51, 54, 60, 81), type= NULL, useLocal = NULL, ...){
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")

  type <- toupper(type)
  if (!is.null(type) && type=="LL"){
    marfGear = c(51)
  }else if (!is.null(type) && type == "HARPOON"){
    marfGear = c(81)
  }else if (!is.null(type) && type == "OTHER"){
    marfGear = c(54,60)
  }
  data = fleet_(fleet = 'SWORDFISHTUNAS', marfSpp = c(251,257,259,379), marfGear = marfGear, isdbSpp = c(72, 73, 190, 191, 192, 846,215,223,230,231,233,234,237,246,592,965,1004), tripcd_id = c(72,73), useLocal = useLocal,...)
  return(data)
}
