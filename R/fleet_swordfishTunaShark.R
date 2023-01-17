#' @title fleet_swordfishTunaShark
#' @description This is a wrapper function that facilitates extracting information for the large
#' pelagics fleet (i.e. swordfish, tuna, shark).
#' All of the information used to identify fleets is stored in the package's associated data files -
#' LIC_CORE, LIC_AREAS, and LIC_GEAR_SPEC.  The various wrappers can have different options (e.g.
#' MOBILE vs FIXED, WESTERN vs EASTERN, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square
#' mesh, etc), and depending on which options are selected, different fleets are identified, and
#' their data is extracted.
#' @param marfGear default is \code{c(51, 54, 60, 81)}. This is a vector of MARFIS gear codes
#' known to have been used by this fleet. The default values can be replaced with a subset of these to
#' only return a gear-specific subset of the fleet's fishing activity.  If other values are provided,
#' the script will not run.
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
#' The following parameters are "softcoded" - any or all of the values can be
#' provided, but other values are not allowed.
#' \itemize{
#'   \item \code{marfGear} = c(51, 54, 60, 81)
#' }
#' @inherit fleet_ details
#' @export
fleet_swordfishTunaShark <- function(marfGear = c(51, 54, 60, 81), type= NULL, useLocal = NULL, socks = FALSE, ...){
  if (!socks) isDraft()
  valuesOK(valSent = NULL, valID = "type", valOK =   c("LL","HARPOON","OTHER"))
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  type <- toupper(type)
  if ((length(type)>0) && type=="LL"){
    marfGear = c(51)
  }else if ((length(type)>0) && type == "HARPOON"){
    marfGear = c(81)
  }else if ((length(type)>0) && type == "OTHER"){
    marfGear = c(54,60)
  }else{
    stop("'type' cannot be NULL")
  }
  valuesOK(valSent = marfGear, valID = "marfGear", valOK =   c(51, 54, 60, 81))
  data = fleet_(fleet = 'SWORDFISHTUNAS', marfSpp = c(251,257,259,379), marfGear = marfGear, isdbSpp = c(72, 73, 190, 191, 192, 846,215,223,230,231,233,234,237,246,592,965,1004), tripcd_id = c(72,73), useLocal = useLocal,...)
  return(data)
}
