#' @title fleet_redfish
#' @description This is a wrapper function that facilitates extracting information for the redfish fleet.
#' All of the information used to identify fleets is stored in the package's associated data files - licCore, licAreas,
#' and licGearSpecs.  The various wrappers can have different options (e.g. mobile vs fixed, western
#' vs eastern, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square mesh, etc), and depending on which options are selected,
#' different fleets are identified, and their data is extracted.
#' @param unit default is \code{NULL}.  Valid selections include \code{"UNIT2"} and \code{"UNIT3"}
#' @param useLocal default is \code{NULL}.
#' @inheritDotParams set_defaults -lics -gearSpecs -area
#' @examples \dontrun{
#' db <- fleet_redfish(unit = "UNIT2",
#'                     useLocal = F,
#'                     year = 2018,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_redfish(unit = "UNIT2",
#'                        useLocal = T,
#'                        year = 2018,
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
#'   }
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfSpp} = 120
#'   \item \code{isdbSpp} = 23
#'   \item \code{tripcd_id} = 23
#' }
#' Licence Information for this fleet is accessible via the following calls:
#' \itemize{
#'   \item \code{Licence Type, Subtype, Gear and Species Information (if applicable)} \code{Mar.fleets::licCore[Mar.fleets::licCore$FLEET=="REDFISH",]}
#'   \item \code{Licence Areas (if applicable)} \code{Mar.fleets::licAreas[Mar.fleets::licAreas$FLEET=="REDFISH",]}
#'
#'   If different areas/components/units are available for this fleet, the areas associated with each can be differentiated by the differing values of \code{FLEET_AREA_ID }..
#'   For example, the Redfish fleet is divided into Units 2 and 3.  All of the NAFO areas associated with either of these units these can be found in via
#'   \code{Mar.fleets::licAreas[Mar.fleets::licAreas$FLEET=="REDFISH",]}, but the NAFO areas associated with the Unit 2 fleet are those with \code{FLEET_AREA_ID} == \code{UNIT2}.
#'   \item \code{Licence Gear Specifications (if applicable)} \code{Mar.fleets::licGearSpecs[Mar.fleets::licGearSpecs$FLEET=="REDFISH",]}
#'
#'    If particular gear size/types are allowed, the range of sizes for each are specified by the MIN and MAX fields.  If aspects of the fleet are defined by the gear size,
#'    multiple records may be present.  For example, the SMALL mesh fleet will have different max and min values than the LARGE MESH fleet.  These records can correspond with
#'    fleet areas, but do not have to.  In this case, the gear associated with catching redfish in UNIT 2 is different than what's allowed in UNIT 3, so the
#'    licGearSpecs table differentiates the gear by having different entries in \code{FLEET_GEARSPECS_ID} (i.e. \code{UNIT2} vs \code{UNIT3}).  The mobile POLLOCK fleet also has multiple
#'    categories of gear sizes, but they are not related to different areas - the entries in \code{FLEET_GEARSPECS_ID} are just \code{SMALL} and \code{LARGE}.
#'    Differing values of Type have not been implemented, but the field exist such that gear can be filtered by Diamond vs Square mesh.
#' }
#' @export
fleet_redfish <- function(unit = NULL, useLocal = NULL, ...){
  unit = toupper(unit)
  if (unit == "UNIT2" | unit == "2" | unit == 2){
    area <- "UNIT2"
    gearSpecs <- "UNIT2"
  } else if (unit == "UNIT3" | unit == "3" | unit == 3){
    area = "UNIT3"
    gearSpecs =  "UNIT3"
  } else{
    stop("Unknown UNIT")
  }

  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")

  data = fleet_(fleet = "REDFISH", marfSpp = 120, isdbSpp = 23, area = area, gearSpecs = gearSpecs, tripcd_id = c(23), useLocal = useLocal,...)
  return(data)
}
