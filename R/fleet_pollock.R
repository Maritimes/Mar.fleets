#' @title fleet_pollock
#' @description This is a wrapper function that facilitates extracting information for the pollock fleet.
#' All of the information used to identify fleets is stored in the package's associated data files -
#' LIC_CORE, LIC_AREAS, and LIC_GEAR_SPEC.  The various wrappers can have different options (e.g.
#' MOBILE vs FIXED, WESTERN vs EASTERN, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square
#' mesh, etc), and depending on which options are selected, different fleets are identified, and
#' their data is extracted.
#' @param marfGear default is \code{c(12,41,51,59)}. This is a vector of MARFIS gear codes
#' known to have been used by this fleet. The default values can be replaced with a subset of these to
#' only return a gear-specific subset of the fleet's fishing activity.  If other values are provided,
#' the script will not run.
#' @param type default is \code{NULL}. This is either "FIXED" or "MOBILE".
#' @param mesh default is \code{'ALL'}. This is either "SMALL" (i.e. 1-129mm) or "LARGE" (i.e. 130mm+), or "ALL".
#' @param area default is \code{'ALL'}. This is either "WESTERN", "EASTERN" or "ALL".
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area
#' @examples \dontrun{
#' db <- fleet_pollock(type = "FIXED",
#'                     mesh = "SMALL",
#'                     area = "WESTERN",
#'                     year = 2018,
#'                     useLocal = F,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_pollock(type = "MOBILE",
#'                        mesh = "LARGE",
#'                        area = "EASTERN",
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
#'   \item \code{marfSpp} =170
#'   \item \code{isdbSpp} = 16
#'   \item \code{tripcd_id} = 7001
#'   \item \code{fleet} = "POLLOCK"
#' }
#' The following parameters are "softcoded" - any or all of the values can be
#' provided, but other values are not allowed.
#' \itemize{
#'   \item \code{marfGear} = c(12,41,51,59)
#' }
#' @inherit fleet_ details
#' @export
fleet_pollock <- function(marfGear = c(12,41,51,59), type = NULL, mesh='ALL', area = "ALL", useLocal = NULL, ...){
  isDraft()
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  type <- toupper(type)
  mesh <- toupper(mesh)
  area <- toupper(area)
  if (!is.null(type) && type =="MOBILE"){
    marfGear = c(12)
  }else if (!is.null(type) && type =="FIXED"){
    marfGear = c(41,51,59)
  }
  valuesOK(valSent = marfGear, valID = "marfGear", valOK =   c(12,41,51,59))
  gearSpecs <- ifelse(mesh == "ALL", "ALL",ifelse(mesh == "SMALL", "SMALL", "LARGE"))
  data <- fleet_(fleet = "POLLOCK", marfSpp = 170, marfGear = marfGear, isdbSpp = 16, area = area, gearSpecs = gearSpecs, tripcd_id = 7001, useLocal = useLocal,...)
  return(data)
}
