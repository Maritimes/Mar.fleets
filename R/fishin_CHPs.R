#' @title fishin_CHPs
#' @description This is a wrapper function that facilitates extracting information for the cod/haddock/pollock fleet.
#' All of the information used to identify fleets is stored in the package's associated data files -
#' LIC_CORE, LIC_AREAS, and LIC_GEAR_SPEC.  The various wrappers can have different options (e.g.
#' MOBILE vs FIXED, WESTERN vs EASTERN, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square
#' mesh, etc), and depending on which options are selected, different fleets are identified, and
#' their data is extracted.
#' @param marfGear default is \code{c('12, 21, 41, 51, 59, 62')}. This is a vector of MARFIS gear codes
#' known to have been used by this fleet. The default values can be replaced with a subset of these to
#' only return a gear-specific subset of the fleet's fishing activity.  If other values are provided,
#' the script will not run.
#' @param type default is \code{NULL}. This is either "FIXED" or "MOBILE".
#' @param stock default is \code{NULL}. This is either "4X5Y" or "5Z".
#' @param areaFileField  default is \code{"COD"}.  This ensures that when set positions from
#' MARFIS and ISDB data are compared to NAFO divisions, they are compared to groundfish-specific
#' divisions of NAFO, including "4X" and "4VsW".
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area
#' @examples \dontrun{
#' db <- fishin_CHPs(type = "FIXED",
#'                     dateStart =  "2016-04-01", dateEnd= "2017-03-31",
#'                     stock = "5Z",
#'                     useLocal = F,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fishin_CHPs(type = "MOBILE",
#'                        dateStart =  "2016-04-01", dateEnd= "2017-03-31",
#'                        stock = "4X5Y",
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
#'   \item \code{marfSpp} = c(100, 110, 170)
#'   \item \code{isdbSpp} = c(10, 11, 16)
#'   \item \code{tripcd_id} = c(10, 7001)
#'   \item \code{fleet} = "CHP"
#' }
#' The following parameters are "softcoded" - any or all of the values can be
#' provided, but other values are not allowed.
#' \itemize{
#'   \item \code{marfGear} = c(12, 21, 41, 51, 59, 62)
#' }
#' @inherit fleet_ details
#' @export
fishin_CHPs <- function(marfGear = c(12, 21, 41, 51, 59, 62), type=NULL, stock = NULL, areaFileField = "COD", useLocal = NULL, socks = FALSE, ...){
  if (!socks) isDraft()
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  type <- toupper(type)
  area <- toupper(stock)
  if (!is.null(type) && type=="MOBILE"){
    marfGear = c(12)
    gearSpecs <- paste0(area,"_MOB")
  }else if (!is.null(type) && type=="FIXED"){
    marfGear <- c(41,51,59)
    gearSpecs <- paste0(area,"_FIXED")
  }
  valuesOK(valSent = marfGear, valID = "marfGear", valOK =   c(12, 21, 41, 51, 59, 62))
  data <- fleet_(fleet = "CHP", marfSpp = c(100, 110, 170), isdbSpp = c(10, 11, 16), marfGear = marfGear, tripcd_id = c(10, 7001), area= area, gearSpecs = gearSpecs, areaFileField = areaFileField, useLocal = useLocal,...)
  return(data)
}
