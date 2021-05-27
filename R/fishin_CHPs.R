#' @title fishin_CHPs
#' @description This is a wrapper function that facilitates extracting information for the cod/haddock/pollock fleet.
#' All of the information used to identify fleets is stored in the package's associated data files - licCore, licAreas,
#' and licGearSpecs.  The various wrappers can have different options (e.g. mobile vs fixed, western
#' vs eastern, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square mesh, etc), and depending on which options are selected,
#' different fleets are identified, and their data is extracted.
#' @param type default is \code{NULL}. This is either "FIXED" or "MOBILE".
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area
#' @examples \dontrun{
#' db <- fishin_CHPs(type = "FIXED",
#'                     year = 2018,
#'                     useLocal = F,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fishin_CHPs(type = "MOBILE",
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
#'   \item \code{tripcd_id} =
#'   \item \code{fleet} =
#'   \item \code{area} =
#'   \item \code{gearSpecs} =
#' }
#' @inherit fleet_ details
#' @export
fishin_CHPs <- function(type=NULL, useLocal = NULL, ...){
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")
  type <- toupper(type)
  if (type=="MOBILE"){
    gearPSOveride <- c(10,12,15)
  }else if (type=="FIXED"){
    gearPSOveride <- c(21,22,41,43,51,52,53,59,98)
  }else{
    stop("Stopping - valid types are 'MOBILE' and 'FIXED'")
  }
  data <- fleet_(fleet = "CHP", marfSpp = c(100, 110, 170), isdbSpp = c(10, 11, 16), tripcd_id = c(10, 7001), area= "4X5YZ", gearPSOveride=gearPSOveride, useLocal = useLocal,...)
  return(data)
}
