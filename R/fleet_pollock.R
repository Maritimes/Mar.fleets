#' @title fleet_pollock
#' @description This is a wrapper function that facilitates extracting information for the pollock fleet.
#' All of the information used to identify fleets is stored in the package's associated data files - licCore, licAreas,
#' and licGearSpecs.  The various wrappers can have different options (e.g. mobile vs fixed, western
#' vs eastern, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square mesh, etc), and depending on which options are selected,
#' different fleets are identified, and their data is extracted.
#' @param type default is \code{NULL}. This is either "FIXED" or "MOBILE".
#' @param mesh default is \code{'ALL'}. This is either "SMALL" (i.e. 1-129mm) or "LARGE" (i.e. 130mm+), or "ALL".
#' @param component default is \code{NULL}. This is either "WESTERN" or "EASTERN".
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
#'   \item \code{marfSpp} =170
#'   \item \code{isdbSpp} = 16
#'   \item \code{tripcd_id} = 7001
#' }
#' Licence Information for this fleet is accessible via the following calls. Fleet information for the MOBILE and FIXED fleets can be specified by using either
#' \code{POLLOCK_MOBILE} or \code{POLLOCK_FIXED}, respectively:
#' \itemize{
#'   \item \code{Licence Type, Subtype, Gear and Species Information (if applicable)} \code{Mar.fleets::licCore[Mar.fleets::licCore$FLEET=="POLLOCK_MOBILE  (or) POLLOCK_FIXED",]}
#'   \item \code{Licence Areas (if applicable)} \code{Mar.fleets::licAreas[Mar.fleets::licAreas$FLEET=="POLLOCK_MOBILE  (or) POLLOCK_FIXED",]}
#'
#'   If different areas/components/units are available for this fleet, the areas associated with each can be differentiated by the differing values of \code{FLEET_AREA_ID }..
#'   For example, the Redfish fleet is divided into Units 2 and 3.  All of the NAFO areas associated with either of these units these can be found in via
#'   \code{Mar.fleets::licAreas[Mar.fleets::licAreas$FLEET=="REDFISH",]}, but the NAFO areas associated with the Unit 2 fleet are those with \code{FLEET_AREA_ID} == \code{UNIT2}.
#'   \item \code{Licence Gear Specifications (if applicable)} \code{Mar.fleets::licGearSpecs[Mar.fleets::licGearSpecs$FLEET=="POLLOCK_MOBILE  (or) POLLOCK_FIXED",]}
#'
#'    If particular gear size/types are allowed, the range of sizes for each are specified by the MIN and MAX fields.  If aspects of the fleet are defined by the gear size,
#'    multiple records may be present.  For example, the SMALL mesh fleet will have different max and min values than the LARGE MESH fleet.  These records can correspond with
#'    fleet areas, but do not have to.  In this case, the gear associated with catching redfish in UNIT 2 is different than what's allowed in UNIT 3, so the
#'    licGearSpecs table differentiates the gear by having different entries in \code{FLEET_GEARSPECS_ID} (i.e. \code{UNIT2} vs \code{UNIT3}).  The mobile POLLOCK fleet also has multiple
#'    categories of gear sizes, but they are not related to different areas - the entries in \code{FLEET_GEARSPECS_ID} are just \code{SMALL} and \code{LARGE}.
#'    Differing values of Type have not been implemented, but the field exist such that gear can be filtered by Diamond vs Square mesh.
#' }
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
