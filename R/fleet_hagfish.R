#' @title fleet_hagfish
#' @description This is a wrapper function that facilitates extracting information for the hagfish fleet.
#' All of the information used to identify fleets is stored in the package's associated data files - licCore, licAreas,
#' and licGearSpecs.  The various wrappers can have different options (e.g. mobile vs fixed, western
#' vs eastern, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square mesh, etc), and depending on which options are selected,
#' different fleets are identified, and their data is extracted.
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area -useLocal
#' @examples \dontrun{
#' db <- fleet_hagfish(useLocal = F,
#'                     year = 2018,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_hagfish(year = 2018,
#'                        useLocal = T,
#'                        data.dir = "c:/data_folder"
#'                       )
#'                        }
#' @family fleets
#' @inherit fleet_ return
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfSpp} = c(197)
#'   \item \code{isdbSpp} = c(241)
#'   \item \code{tripcd_id} = c(7063)
#'   \item \code{fleet} = "HAGFISH"
#'   \item \code{gearPSOveride} = 61 (licence documents indicate that gear type is 62, but catch information all uses 61)
#' }
#' @inherit fleet_ details
#' @export
fleet_hagfish <- function(useLocal = NULL, ...){
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
    data = fleet_(fleet = "HAGFISH", marfSpp = c(197), isdbSpp = c(241), gearPSOveride = 61, tripcd_id = c(7063), useLocal = useLocal,...)
  return(data)
}
