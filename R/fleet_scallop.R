#' @title fleet_scallop
#' @description This is a wrapper function that facilitates extracting information for the scallop fleet.
#' All of the information used to identify fleets is stored in the package's associated data files - licCore, licAreas,
#' and licGearSpecs.  The various wrappers can have different options (e.g. mobile vs fixed, western
#' vs eastern, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square mesh, etc), and depending on which options are selected,
#' different fleets are identified, and their data is extracted.
#' @param fleet default is NULL.  Valid values are "INSHORE" or "OFFSHORE"
#' @param useLocal default is \code{NULL}.  Valid selections are  \code{TRUE} and \code{FALSE}
#'
#' if \code{TRUE}, then the following parameter is necessary:
#' ' \itemize{
#'   \item \code{data.dir} - This is a path on your computer to where your .RData files are stored.
#'   }
#'
#' if \code{FALSE}, then the following parameters are necessary:
#' ' \itemize{
#'   \item \code{oracle.username} - your existing oracle login name
#'   \item \code{oracle.password} - your existing oracle password
#'   \item \code{oracle.dsn} - usually "PTRAN" - the name of the database you're connecting to
#'   \item \code{usepkg} - default is \code{"rodbc"}, but \code{"roracle"} is also valid
#'   }
#' @inheritDotParams set_defaults -lics -gearSpecs -area
#' @examples \dontrun{
#' db <- fleet_scallop(fleet = "INSHORE",
#'                     year = 2018,
#'                     useLocal = F,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_scallop(fleet = "OFFSHORE",
#'                        year = 2018,
#'                        useLocal = T,
#'                        data.dir = "c:/data_folder"
#'                       )
#'                        }
#' @family fleets
#' @return specific returned objects can be specified by the user, but the default result is a list of objects, including marfis data, isdb data, information for matching isdb
#' and marfis data, and a summary of bycatch, specifically:
#' a list item containing:
#' \itemize{
#'   \item \code{FLEET} - This is a dataframe of the unique combinations of (MARFIS) LICENCE_ID, VR_NUMBER and GEAR_CODE that
#'   was found for this fleet during the specified period
#'   \item \code{FLEET_ACTIVITY} - This is a dataframe of identifiers for all of the (MARFIS) fishing activity undertaken
#'   by vessels of this fleet during the specified period (i.e. LICENCE_ID, PRO_SPC_INFO_ID, LOG_EFRT_STD_INFO_ID, GEAR_CODE,
#'   MON_DOC_ID, VR_NUMBER, and several dates associated with the trip)
#'   \item \code{marf} - This is a list of 3 sets of information for the commercial catch data (i.e. marfis)-
#'   the trips, the sets, and a special dataframe containing information that can be used to link
#'   the commercial data to the ISDB data
#'   \item \code{isdb} - This is a list of 4 data objects - 2 of which are all of the discovered ISDB data
#'   TRIPS and SETS for the fleet, as well as the TRIPS and SETS from the observer data that were
#'   sucessfully matched with the MARFIS data
#'   \item \code{bycatch} - This is a dataframe with the various species that were observed during observed
#'   trips.  For each species, the estimated number caught, the estimated kept wt (kgs) and the
#'   estimated discarded wt(kg) are all captured
#'   }
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{a df - more dets necessary}
#'   \item \code{gearCode} = c(71)
#'   \item \code{marfSpp} = 612
#' }
#' @export
fleet_scallop <- function(fleet = NULL, useLocal = NULL, ...){
  fleet <- toupper(fleet)
  fleet <- ifelse(fleet == "OFFSHORE", "SCALLOP_OFF", "SCALLOP_INSH")
  data <- fleet_(fleet = fleet, marfSpp = 612, area = "ALL", gearSpecs = "ALL", useLocal = useLocal,...)
  return(data)
}
