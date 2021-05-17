#' @title fleet_silverhake
#' @description This is a wrapper function that facilitates extracting information for the silver hake fleet.
#' All of the information used to identify fleets is stored in the package's associated data files - licCore, licAreas,
#' and licGearSpecs.  The various wrappers can have different options (e.g. mobile vs fixed, western
#' vs eastern, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square mesh, etc), and depending on which options are selected,
#' different fleets are identified, and their data is extracted.
#' @param mesh default is \code{'ALL'}. This indicates whether or not the the
#' fleet should be filtered by mesh size.  It can be either "ALL" or "SMALL"  (i.e. 55-65mm).
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area
#' @examples \dontrun{
#' db <- fleet_silverhake(mesh = "SMALL",
#'                     year = 2018,
#'                     useLocal = F,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_silverhake(mesh = "ALL",
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
#'   \item \code{marfSpp} = 172
#'   \item \code{isdbSpp} = 14
#'   \item \code{tripcd_id} = c(14,7002)
#'   \item \code{fleet} = "SHAKE"
#'   \item \code{gearSpecs} = "ALL" or "SMALL", depending on selections
#' }
#' @inherit fleet_ details
#' @export
fleet_silverhake <- function(mesh='ALL', useLocal=NULL, ...){
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  mesh <- toupper(mesh)
  gearSpecs <- ifelse(mesh == "ALL", "ALL","SMALL")
  data <- fleet_(fleet = "SHAKE", marfSpp = 172, isdbSpp = 14, gearSpecs = gearSpecs, tripcd_id = c(14,7002), useLocal = useLocal,...)
  return(data)
  }

