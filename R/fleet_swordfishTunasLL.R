#' @title fleet_swordfishTunasLL
#' @description This is a wrapper function that facilitates extracting information for the large pelagics longline fleet.
#' All of the information used to identify fleets is stored in the package's associated data files - licCore, licAreas,
#' and licGearSpecs.  The various wrappers can have different options (e.g. mobile vs fixed, western
#' vs eastern, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square mesh, etc), and depending on which options are selected,
#' different fleets are identified, and their data is extracted.
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area -useLocal
#' @examples \dontrun{
#' db <- fleet_swordfishTunasLL(useLocal = F,
#'                     year = 2018,
#'                     oracle.username = "<name>",
#'                     oracle.password="<password>",
#'                     oracle.dsn="PTRAN",
#'                     usepkg = "roracle"
#'                     )
#' local <- fleet_swordfishTunasLL(year = 2018,
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
#'   \item \code{isdbSpp} = c(72, 73, 190, 191, 192, 846)
#'   \item \code{tripcd_id} = c(72,73, 7059)
#' }
#' Licence Information for this fleet is accessible via the following calls:
#' \itemize{
#'   \item \code{Licence Type, Subtype, Gear and Species Information (if applicable)} \code{Mar.fleets::licCore[Mar.fleets::licCore$FLEET=="SWORDFISHTUNASLL",]}
#'   \item \code{Licence Areas (if applicable)} \code{Mar.fleets::licAreas[Mar.fleets::licAreas$FLEET=="SWORDFISHTUNASLL",]}
#'
#'   If different areas/components/units are available for this fleet, the areas associated with each can be differentiated by the differing values of \code{FLEET_AREA_ID }..
#'   For example, the Redfish fleet is divided into Units 2 and 3.  All of the NAFO areas associated with either of these units these can be found in via
#'   \code{Mar.fleets::licAreas[Mar.fleets::licAreas$FLEET=="REDFISH",]}, but the NAFO areas associated with the Unit 2 fleet are those with \code{FLEET_AREA_ID} == \code{UNIT2}.
#'   \item \code{Licence Gear Specifications (if applicable)} \code{Mar.fleets::licGearSpecs[Mar.fleets::licGearSpecs$FLEET=="HALIBUT",]}
#'
#'    If particular gear size/types are allowed, the range of sizes for each are specified by the MIN and MAX fields.  If aspects of the fleet are defined by the gear size,
#'    multiple records may be present.  For example, the SMALL mesh fleet will have different max and min values than the LARGE MESH fleet.  These records can correspond with
#'    fleet areas, but do not have to.  In this case, the gear associated with catching redfish in UNIT 2 is different than what's allowed in UNIT 3, so the
#'    licGearSpecs table differentiates the gear by having different entries in \code{FLEET_GEARSPECS_ID} (i.e. \code{UNIT2} vs \code{UNIT3}).  The mobile POLLOCK fleet also has multiple
#'    categories of gear sizes, but they are not related to different areas - the entries in \code{FLEET_GEARSPECS_ID} are just \code{SMALL} and \code{LARGE}.
#'    Differing values of Type have not been implemented, but the field exist such that gear can be filtered by Diamond vs Square mesh.
#' }
#' @export
fleet_swordfishTunasLL <- function(useLocal = NULL, ...){
  if(!paramOK(useLocal = useLocal, p=list(...))) stop("Please provide additional parameters as directed above")

  data = fleet_(fleet = "SWORDFISHTUNASLL", marfSpp = c(251,257,259,379), isdbSpp = c(72, 73, 190, 191, 192, 846,215,223,230,231,233,234,237,246,592,965,1004), area = "ALL", tripcd_id = c(72,73), useLocal = useLocal,...)
  return(data)
}
