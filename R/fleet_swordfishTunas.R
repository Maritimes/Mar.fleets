#' @title fleet_swordfishTunas
#' @description This is a wrapper function that facilitates extracting information for the large pelagics fleet.
#' All of the information used to identify fleets is stored in the package's associated data files - licCore, licAreas,
#' and licGearSpecs.  The various wrappers can have different options (e.g. mobile vs fixed, western
#' vs eastern, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square mesh, etc), and depending on which options are selected,
#' different fleets are identified, and their data is extracted.
#' @param type default is \code{NULL}. This is either "LL" (longline) or "HARPOON".
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area -useLocal
#' @examples \dontrun{
#' db <- fleet_swordfishTunas(useLocal = F,
#'                     year = 2018,
#'                     type= "HARPOON",
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_swordfishTunas(year = 2018,
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
fleet_swordfishTunas <- function(useLocal = NULL, fleet = NULL, ...){
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  fleet <-toupper(fleet)
    if (fleet %in% c("LL","LONGLINE")){
      fleet = "SWORDFISHTUNASLL"
    }else if (fleet %in% c("HARPOON")){
      fleet = "SWORDFISHTUNASHARP"
    }else{
      stop("Specified type is invalid")
    }

  data = fleet_(fleet = fleet, marfSpp = c(251,257,259,379), isdbSpp = c(72, 73, 190, 191, 192, 846,215,223,230,231,233,234,237,246,592,965,1004), tripcd_id = c(72,73), useLocal = useLocal,...)
  return(data)
}
