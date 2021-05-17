#' @title fleet_halibut
#' @description This is a wrapper function that facilitates extracting information for the halibut fleet.
#' All of the information used to identify fleets is stored in the package's associated data files - licCore, licAreas,
#' and licGearSpecs.  The various wrappers can have different options (e.g. mobile vs fixed, western
#' vs eastern, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square mesh, etc), and depending on which options are selected,
#' different fleets are identified, and their data is extracted.
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area -useLocal
#' @examples \dontrun{
#' db <- fleet_halibut(useLocal = F,
#'                     year = 2018,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_halibut(year = 2018,
#'                        useLocal = T,
#'                        data.dir = "c:/data_folder"
#'                       )
#'                        }
#' @family fleets
#' @inherit fleet_ return
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfSpp} = 130
#'   \item \code{isdbSpp} = 30
#'   \item \code{nafoCode} = "ALL", which, for this fleet is c('3N\%','3O\%','3PS\%','4V\%','4W\%','4X\%','5\%')
#'   \item \code{tripcd_id} = c(30,7057,7058)
#'   \item \code{fleet} = "HALIBUT"
#'   \item \code{areaFile} == "Areas_Halibut_sf"
#' }
#' @inherit fleet_ details
#' @export
fleet_halibut <- function(useLocal = NULL, ...){
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")

  data = fleet_(fleet = "HALIBUT", marfSpp = 130, isdbSpp = 30, area = "ALL", tripcd_id = c(30,7057,7058), areaFile = 'Areas_Halibut_sf', useLocal = useLocal,...)
  return(data)
}
