#' @title fleet_pollock
#' @description This is a wrapper function that facilitates extracting information for the pollock fleet.
#' All of the information used to identify fleets is stored in the package's associated data files - licCore, licAreas,
#' and licGearSpecs.  The various wrappers can have different options (e.g. mobile vs fixed, western
#' vs eastern, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square mesh, etc), and depending on which options are selected,
#' different fleets are identified, and their data is extracted.
#' @param type default is \code{NULL}. This is either "FIXED" or "MOBILE".
#' @param mesh default is \code{'ALL'}. This is either "SMALL" (i.e. 1-129mm) or "LARGE" (i.e. 130mm+), or "ALL".
#' @param component default is \code{NULL}. This is either "WESTERN" or "EASTERN".
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area
#' @examples \dontrun{
#' db <- fleet_pollock(type = "FIXED",
#'                     mesh = "SMALL",
#'                     component = "WESTERN",
#'                     year = 2018,
#'                     useLocal = F,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_pollock(type = "MOBILE",
#'                        mesh = "LARGE",
#'                        component = "EASTERN",
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
#'   \item \code{fleet} = "POLLOCK_MOBILE" or "POLLOCK_FIXED", depending on selections
#'   \item \code{area} = "WESTERN" or "EASTERN", depending on selections
#'   \item \code{gearSpecs} = "ALL", "SMALL" or "LARGE", depending on selections
#' }
#' @inherit fleet_ details
#' @export
fleet_pollock <- function(type = NULL, mesh='ALL', component = NULL, useLocal = NULL, ...){
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  type <- toupper(type)
  mesh <- toupper(mesh)
  component <- toupper(component)

  fleet <- ifelse(type == "MOBILE", "POLLOCK_MOBILE", "POLLOCK_FIXED")
  gearSpecs <- ifelse(mesh == "ALL", "ALL",ifelse(mesh == "SMALL", "SMALL", "LARGE"))
  area <- ifelse(component == "WESTERN", "WESTERN", "EASTERN")
  data <- fleet_(fleet = fleet, marfSpp = 170, isdbSpp = 16, area = area, gearSpecs = gearSpecs, tripcd_id = c(7001), useLocal = useLocal,...)
  return(data)
}
