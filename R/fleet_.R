#' @title fleet_
#' @description This function is a generic wrapper that facilitates extracting fleet specific
#' information using the data provided by fleet-specific wrappers.
#' @param fleet This is an identifier that will be used to interrogate the LIC_CORE object to select
#' the correct type(s), subtype(s), gear(s), and licence_species for the selected fleet
#' @param area This is an identifier that will be used to interrogate the LIC_AREAS object to select
#' any NAFO areas associated with the selected fleet
#' @param gearSpecs This is an identifier that will be used to interrogate the LIC_GEAR_SPEC object
#' to select any gear parameters associated with the selected fleet
#' @inheritDotParams set_defaults
#' @examples \dontrun{
#' test <- fleet_(fleet="POLLOCK_MOBILE", area="WESTERN", gearSpecs="SMALL" )
#'  }
#' @details
#'  Licence Information for any fleet is accessible via the following calls.  Please replace
#'  "<fleet>", with this fleet's actual fleet value identified in the NOTE, below:
#' \itemize{
#'   \item \code{Licence Type, Subtype, Gear and Species Information (if applicable)}
#'   \code{Mar.fleets::LIC_CORE[Mar.fleets::LIC_CORE$FLEET=="<fleet>",]}
#'   \item \code{Licence Areas (if applicable)} \code{Mar.fleets::LIC_AREAS[Mar.fleets::LIC_AREAS$FLEET=="<fleet>",]}
#'
#'   If different areas/components/units are available for this fleet, the areas associated with
#'   each can be differentiated by the differing values of \code{FLEET_AREA_ID }. For example, the
#'   Redfish fleet is divided into Units 2 and 3.  All of the NAFO areas associated with either of
#'   these units these can be found in via \code{Mar.fleets::LIC_AREAS[Mar.fleets::LIC_AREAS$FLEET=="REDFISH",]},
#'   but the NAFO areas associated with the Unit 2 fleet are those with \code{FLEET_AREA_ID} == \code{UNIT2}.
#'   \item \code{Licence Gear Specifications (if applicable)} \code{Mar.fleets::LIC_GEAR_SPEC[Mar.fleets::LIC_GEAR_SPEC$FLEET=="<fleet>",]}
#'
#'    If particular gear size/types are allowed, the range of sizes for each are specified by the
#'    MIN and MAX fields.  If aspects of the fleet are defined by the gear size, multiple records
#'    may be present.  For example, the SMALL mesh fleet will have different max and min values than
#'    the LARGE MESH fleet.  These records can correspond with fleet areas, but do not have to.  In
#'    this case, the gear associated with catching redfish in UNIT 2 is different than what's
#'    allowed in UNIT 3, so the LIC_GEAR_SPEC table differentiates the gear by having different
#'    entries in \code{FLEET_GEARSPECS_ID} (i.e. \code{UNIT2} vs \code{UNIT3}).  The mobile POLLOCK
#'    fleet also has multiple categories of gear sizes, but they are not related to different areas
#'    - the entries in \code{FLEET_GEARSPECS_ID} are just \code{SMALL} and \code{LARGE}.  Differing
#'    values of Type have not been implemented, but the field exist such that gear can be filtered
#'    by Diamond vs Square mesh.
#' }
#' @family coreFuncs
#' @return specific returned objects can be specified by the user, but the default result is a list
#' of objects.  The list includes marfis data, isdb data, information related to the matching, and a
#' breakdown of where the various trips and sets occurred, specifically:
#' \itemize{
#'  \item \code{params} -  this is a list containing information about the extraction
#'  \itemize{
#'  \item \code{user} - this contain all of the parameters sent to the function (including defaults, user-provided and hardcoded)
#'  \item \code{fleet} - this is a list object containing 3 dataframes that contain the information used to identify the fleet.
#'  These will include licencesCore, licecesAreas, and licencesGearSpecs.  Depending on how the fleet is defined, one or more of
#'  these may be empty.
#'  }
#'   \item \code{fleet} - This is a dataframe of the unique combinations of (MARFIS) LICENCE_ID, VR_NUMBER and GEAR_CODE that
#'   was found for this fleet during the specified period
#'   \item \code{FLEET_ACTIVITY} - This is a dataframe of identifiers for all of the (MARFIS) fishing activity undertaken
#'   by vessels of this fleet during the specified period (i.e. LICENCE_ID, PRO_SPC_INFO_ID, LOG_EFRT_STD_INFO_ID, GEAR_CODE,
#'   MON_DOC_ID, VR_NUMBER, and several dates associated with the trip)
#'   \item \code{marf} - This is a list of 3 sets of information for the commercial catch data (i.e. marfis):
#'   \itemize{
#'   \item \code{MARF_TRIPS}
#'   \item \code{MARF_SETS}
#'   \item \code{MARF_MATCH} This is a special dataframe containing information that can be used to link
#'   the commercial data to the ISDB data
#'   }
#'   \item \code{isdb} - This is a list of data objects from the ISDB db:
#'   \itemize{
#'   \item \code{ISDB_TRIPS} These are ISDB trips that are associated with MARFIS trips from the \code{marf$MARF_TRIPS} object above
#'   \item \code{ISDB_SETS} These are all of the ISDB sets associated with the ISDB_TRIPS (matched and unmatched)
#'   \item \code{ISDB_CATCHES} This is the data associated with the records in ISDB_TRIPS
#'   \itemize{
#'   \item \code{ALL} This is the raw data from ISCATCHES for the trips found in ISDB_TRIPS
#'   \item \code{SUMMARY} This is the data from ISCATCHES for all of the trips found in ISDB_TRIPS, summarized by species. Each species
#'   has calculated aggregate values for "EST_NUM_CAUGHT", EST_KEPT_WT", "EST_DISCARD_WT" and "EST_COMBINED_WT"
#'   }
#'   }
#'   \item \code{matches} This is a list item that contains all of the information used to assigne matches between MARFIS and ISDB
#'   \itemize{
#'   \item \code{MATCH_SUMMARY_TRIPS} This is a simple breakdown of the various approaches used for matching, and the relative success of each.
#'   Matches can occur using multiple approaches, so these can not be added up.  This list also includes "\code{Likely_Swapped_VR_Lic}" which
#'   indicates how may matches seem to have the values for LICENCE_ID and VR_NUMBER reversed, and includes the count of how many rows are present
#'   in both \code{Multimatches} and \code{Umatchables}
#'   \item \code{MATCH_DETAILS} This is a dataframe of all of the MARFIS and ISDB trips that have been associated with each other, and whether
#'   or not they were matched on each of the possible approaches
#'   \item \code{ISDB_UNMATCHABLES} These are the trips from MARFIS that included ISDB-type information (e.g. Observer ID, ISDB Trip name, etc), but
#'   for which no ISDB match could be found.
#'   \item \code{ISDB_MULTIMATCHES} These are ISDB trips that were found to be match multiple MARFIS trips equally well.
#'   }
#'   \item \code{location_summary} - This is a list of 1 or more dataframes that breaks down the various trips
#'   and sets by the areas in which they occurred.  NAFO locations are reported for MARFIS trips, MARFIS sets and ISDB sets
#'   (not ISDB trips).  These reported locations are shown, as are the "calculated" locations, which are based on the
#'   reported latitudes and longitudes. No "calculated" locations are shown for MARFIS trips, as there are no coordinates for
#'   the trip level.  If a custom value for \code{areaFile} was sent (i.e. not "NAFOSubunits_sf"), a second dataframe breaking
#'   down the sets by the custom area will also be provided.
#'   }
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
fleet_ <- function(fleet=NULL, area = NULL, gearSpecs = NULL, ...){
  # if (!go()){
  #   message("Sorry - this package is draft.  Please contact Mike.McMahon@dfo-mpo.gc.ca if you want to try it out it")
  #   stop()
  # }
  if (is.null(fleet)){
    stop("Please provide a fleet")
  }
  dbEnv<-NA
  if (!exists("dbEnv", envir = .GlobalEnv)) assign("dbEnv", new.env(), envir = .GlobalEnv)
  lics <- LIC_CORE[LIC_CORE$FLEET==fleet,]
  if (nrow(lics)==0)stop("No licences found - stopping")
  if(any(is.null(area) | length(area)==0)){
    area <-  LIC_AREAS[FALSE,]
  } else {
    area <-  LIC_AREAS[LIC_AREAS$FLEET == fleet & LIC_AREAS$FLEET_AREA_ID == area,]
  }
  if (is.null(gearSpecs)){
    gearSpecs <- LIC_GEAR_SPEC[FALSE,]
  }else{
    gearSpecs <- LIC_GEAR_SPEC[LIC_GEAR_SPEC$FLEET == fleet & LIC_GEAR_SPEC$FLEET_GEARSPECS_ID == gearSpecs, ]
  }
  rm(list = c("fleet","dbEnv"))

  argsFn <- as.list(environment())

  # grab user submitted and combine -------------------------------------------------------------------------------------------------------------------------
  argsUser <- list(...)

  # add remaining default args ------------------------------------------------------------------------------------------------------------------------------
  args <- do.call(set_defaults, list(argsFn=argsFn, argsUser=argsUser))

  params <- args$params
  # Verify we have necessary data/permissions ---------------------------------------------------------------------------------------------------------------
  args <- do.call(can_run, args$args)

  if (args$debug) Mar.utils::where_now()

  #set up results list, and populate according to arguments
  data <- list()
  data[["params"]]<-list()
  data$params[["user"]]<-params
  data$params[["fleet"]]<-list()
  data$params$fleet[["licencesCore"]] <- lics
  data$params$fleet[["licencesAreas"]] <- area
  data$params$fleet[["licencesGearSpecs"]] <- gearSpecs
  fleet <- do.call(get_fleet, args)
  data$params$fleet$licencesCore <- merge(data$params$fleet$licencesCore, fleet$LICDETS[,c("LICENCE_TYPE_ID", "LICENCE_TYPE")], by.x="LIC_TYPE", by.y="LICENCE_TYPE_ID", all.x=T)
  data$params$fleet$licencesCore <-merge(data$params$fleet$licencesCore, fleet$LICDETS[,c("LICENCE_SUBTYPE_ID", "LICENCE_SUBTYPE")], by.x="LIC_SUBTYPE", by.y="LICENCE_SUBTYPE_ID", all.x=T)
  # data$params$fleet$licencesCore <-merge(data$params$fleet$licencesCore, fleet$LICDETS[,c("GEAR_CODE", "GEAR")], by.x="LIC_GEAR", by.y="GEAR_CODE", all.x=T)
  data$params$fleet$licencesCore <-merge(data$params$fleet$licencesCore, fleet$LICDETS[,c("SPECIES_CODE", "SPECIES")], by.x="LIC_SP", by.y="SPECIES_CODE", all.x=T)
  data$params$fleet$licencesCore <- unique(data$params$fleet$licencesCore)
  fleet$LICDETS <- NULL
  data[["fleet"]]<- fleet
  data$fleet$debug <-NULL

  if (args$returnMARFIS){
    marf <- do.call(get_marfis, list(thisFleet=fleet$FLEET_ACTIVITY,args=args))
    data[["marf"]]<- marf

    data$marf$debug <-NULL
    if (args$returnISDB){
      isdb <- do.call(get_isdb, list(thisFleet=fleet$FLEET_ACTIVITY,get_marfis = marf, args=args))

      if (length(isdb)>1 && class(isdb$ISDB_TRIPS)=="data.frame"){

        data[["isdb"]]<- isdb
        data$isdb$debug <-NULL
        loc <- do.call(summarize_locations, list(get_isdb = isdb, get_marfis = marf, args=args))
        data[["location_summary"]]<- loc

        data[["matches"]]<-list()
        data$matches[["MATCH_SUMMARY_TRIPS"]] <- data$isdb$MATCH_SUMMARY_TRIPS
        data$isdb$MATCH_SUMMARY_TRIPS <- NULL
        data$matches[["MATCH_DETAILS"]] <- data$isdb$MATCH_DETAILS
        data$isdb$MATCH_DETAILS <- NULL
        data$matches[["ISDB_UNMATCHABLES"]] <- data$isdb$ISDB_UNMATCHABLES
        data$isdb$ISDB_UNMATCHABLES <- NULL
        data$matches[["ISDB_MULTIMATCHES"]] <- data$isdb$ISDB_MULTIMATCHES
        data$isdb$ISDB_MULTIMATCHES <- NULL
      }
    }
  }
  if (!(class(dbEnv$debugLics) == "NULL" &&
        class(dbEnv$debugVRs) == "NULL" &&
        class(dbEnv$debugMARFTripIDs) == "NULL" &&
        class(dbEnv$debugISDBTripIDs) == "NULL"  &&
        class(dbEnv$debugISDBTripNames) == "NULL")){
    data[["debug"]]<- list()
    if (!class(dbEnv$debugLics) == "NULL") data$debug$debugLics <- dbEnv$debugLics
    if (!class(dbEnv$debugVRs) == "NULL") data$debug$debugVRs <- dbEnv$debugVRs
    if (!class(dbEnv$debugMARFTripIDs) == "NULL") data$debug$debugMARFTripIDs <- dbEnv$debugMARFTripIDs
    if (!class(dbEnv$debugISDBTripIDs) == "NULL") data$debug$debugISDBTripIDs <- dbEnv$debugISDBTripIDs
    if (!class(dbEnv$debugISDBTripNames) == "NULL") {
      tmp <- merge(dbEnv$debugISDBTripNamesLookup, dbEnv$debugISDBTripNames, by.x = "ISDB_TRIP_CLN", by.y = "expected")
      tmp = tmp[,2:ncol(tmp)]
      data$debug$debugISDBTripNames <- tmp
    }
  }
  rm(dbEnv, envir = .GlobalEnv)
  return(data)
}
