#' @title fleet_halibut
#' @description This is a wrapper function that facilitates extracting information for the halibut fleet.
#' All of the information used to identify fleets is stored in the package's associated data files -
#' LIC_CORE, LIC_AREAS, and LIC_GEAR_SPEC.  The various wrappers can have different options (e.g.
#' MOBILE vs FIXED, WESTERN vs EASTERN, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square
#' mesh, etc), and depending on which options are selected, different fleets are identified, and
#' their data is extracted.
#' @param marfGear default is \code{c(12, 41, 51, 59)}. This is a vector of MARFIS gear codes known to have caught
#' this species. The default values can be replaced with a smaller selection to only return information
#' for a gear-specific subset of fishing activity.
#' @param area default is \code{c("3NOPS4VWX")}.  This restricts the data to particular NAFO areas.
#' Setting it to "ALL" or NULL removes the NAFO restrictions are returns all results.
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
fleet_halibut <- function(marfGear = c(12, 41, 51, 59), area= "3NOPS4VWX", useLocal = NULL, ...){
  if (!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  if (is.null(area) || area =="ALL")area =="ALL"
  data = fleet_(fleet = "HALIBUT", marfSpp = 130, marfGear = marfGear, isdbSpp = 30, area = area, tripcd_id = c(30,7057,7058), areaFile = 'Areas_Halibut_sf', useLocal = useLocal,...)
  return(data)
}
