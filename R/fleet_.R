#' @title fleet_
#' @description This function is a generic wrapper that facilitates extracting fleet specific information using the data provided by fleet-specific wrappers.
#' @param fleet This is an identifier that will be used to interrogate the licCore object to select the correct type(s), subtype(s), gear(s), and licence_species for the selected fleet
#' @param area This is an identifier that will be used to interrogate the licAreas object to select any NAFO areas associated with the selected fleet
#' @param gearSpecs This is an identifier that will be used to interrogate the licGearSpecs object to select any gear parameters associated with the selected fleet
#' @inheritDotParams set_defaults
#' @examples \dontrun{
#' test <- fleet_(fleet="POLLOCK_MOBILE", area="WESTERN", gearSpecs="SMALL" )
#'  }
#' @family fleets
#' @return list of objects, including marfis data, isdb data, information for matching isdb
#' and marfis data, and a summary of bycatch
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
fleet_ <- function(fleet=NULL, area = NULL, gearSpecs = NULL, ...){

  if (is.null(fleet)){
    stop("Please provide a fleet")
  }
  # get the fleet's parameters
  utils::data("licCore", envir = environment())
  utils::data("licAreas", envir = environment())
  utils::data("licGearSpecs", envir = environment())

  licCore <- get("licCore", envir  = environment())
  licAreas <- get("licAreas", envir  = environment())
  licGearSpecs <- get("licGearSpecs", envir  = environment())


  lics <- licCore[licCore$FLEET==fleet,]
  if(is.null(area)){
    area <-  licAreas[FALSE,]
  } else {
    area <-  licAreas[licAreas$FLEET == fleet & licAreas$FLEET_AREA_ID == area,]
  }
  if (is.null(gearSpecs)){
    gearSpecs <- licGearSpecs[FALSE,]
  }else{
    gearSpecs <- licGearSpecs[licGearSpecs$FLEET == fleet & licGearSpecs$FLEET_GEARSPECS_ID == gearSpecs, ]
  }

  rm(list = c("licCore", "licAreas", "licGearSpecs", "fleet"))

  argsFn <- as.list(environment())

  # grab user submitted and combine -------------------------------------------------------------------------------------------------------------------------
  argsUser <- list(...)

  # add remaining default args ------------------------------------------------------------------------------------------------------------------------------
  args <- do.call(set_defaults, list(argsFn=argsFn, argsUser=argsUser))

  params <- args$params
  # Verify we have necessary data/permissions ---------------------------------------------------------------------------------------------------------------
  args <- do.call(can_run, args$args)

  if (args$debuggit) Mar.utils::where_now()

  #set up results list, and populate according to arguments
  data <- list()
  data[["params"]]<-list()
  data$params[["user"]]<-params
  data$params[["fleet"]]<-list()
  data$params$fleet[["licencesCore"]] <- lics
  data$params$fleet[["licencesAreas"]] <- area
  data$params$fleet[["licencesGearSpecs"]] <- gearSpecs
  if (args$returnFleet){
    fleet <- do.call(get_fleet, args)
    data[["fleet"]]<- fleet
  }
  if (!class(fleet$debug$debugLics) == "NULL") args$debugLics <- fleet$debug$debugLics
  if (!class(fleet$debug$debugVRs) == "NULL") args$debugVRs <- fleet$debug$debugVRs
  if (!class(fleet$debug$debugMARFTripIDs) == "NULL") args$debugMARFTripIDs <- fleet$debug$debugMARFTripIDs
  if (!class(fleet$debug$debugISDBTripIDs) == "NULL") args$debugISDBTripIDs <- fleet$debug$debugISDBTripIDs
  if (!class(fleet$debug$debugISDBTripNames) == "NULL") args$debugISDBTripNames <- fleet$debug$debugISDBTripNames
  data$fleet$debug <-NULL

  if (args$returnMARFIS){
    marf <- do.call(get_marfis, list(thisFleet=fleet$FLEET_ACTIVITY,args=args))
    data[["marf"]]<- marf
    if (!class(marf$debug$debugLics) == "NULL") args$debugLics <- marf$debug$debugLics
    if (!class(marf$debug$debugVRs) == "NULL") args$debugVRs <- marf$debug$debugVRs
    if (!class(marf$debug$debugMARFTripIDs) == "NULL") args$debugMARFTripIDs <- marf$debug$debugMARFTripIDs
    if (!class(marf$debug$debugISDBTripIDs) == "NULL") args$debugISDBTripIDs <- marf$debug$debugISDBTripIDs
    if (!class(marf$debug$debugISDBTripNames) == "NULL") args$debugISDBTripNames <- marf$debug$debugISDBTripNames
    data$marf$debug <-NULL
    if (args$returnISDB){
      isdb <- do.call(get_isdb, list(thisFleet=fleet$FLEET_ACTIVITY,get_marfis = marf, matchMarfis = T, args=args))

      if(args$dropUnmatchedISDB){
        isdb$ISDB_TRIPS <- isdb$ISDB_TRIPS[!is.na(isdb$ISDB_TRIPS$TRIP_ID_MARF),]
        isdb$ISDB_SETS <- isdb$ISDB_SETS[!is.na(isdb$ISDB_SETS$TRIP_ID_MARF),]
      }

      if (length(isdb)>1 && class(isdb$ISDB_TRIPS)=="data.frame"){
        data[["isdb"]]<- isdb
        if (!class(isdb$debug$debugLics) == "NULL") args$debugLics <- isdb$debug$debugLics
        if (!class(isdb$debug$debugVRs) == "NULL") args$debugVRs <- isdb$debug$debugVRs
        if (!class(isdb$debug$debugMARFTripIDs) == "NULL") args$debugMARFTripIDs <- isdb$debug$debugMARFTripIDs
        if (!class(isdb$debug$debugISDBTripIDs) == "NULL") args$debugISDBTripIDs <- isdb$debug$debugISDBTripIDs
        if (!class(isdb$debug$debugISDBTripNames) == "NULL") args$debugISDBTripNames <- isdb$debug$debugISDBTripNames
        data$isdb$debug <-NULL

        if (args$returnBycatch){
          bycatch <- do.call(get_bycatch, list(isTrips = unique(isdb$ISDB_TRIPS[!is.na(isdb$ISDB_TRIPS$TRIP_ID_MARF), "TRIP_ID_ISDB"]), args=args))
          data[["bycatch"]]<- bycatch
        }

        if (args$returnLocations){
          loc <- do.call(summarize_locations, list(get_isdb = isdb, get_marfis = marf, args=args))
          data[["location_summary"]]<- loc
        }
      }
    }
  }
  if (!(class(args$debugLics) == "NULL" &&
        class(args$debugVRs) == "NULL" &&
        class(args$debugMARFTripIDs) == "NULL" &&
        class(args$debugISDBTripIDs) == "NULL"  &&
        class(args$debugISDBTripNames) == "NULL")){
    data[["debug"]]<- list()
    if (!class(args$debugLics) == "NULL") data$debug$debugLics <- args$debugLics
    if (!class(args$debugVRs) == "NULL") data$debug$debugVRs <- args$debugVRs
    if (!class(args$debugMARFTripIDs) == "NULL") data$debug$debugMARFTripIDs <- args$debugMARFTripIDs
    if (!class(args$debugISDBTripIDs) == "NULL") data$debug$debugISDBTripIDs <- args$debugISDBTripIDs
    if (!class(args$debugISDBTripNames) == "NULL") data$debug$debugISDBTripNames <- args$debugISDBTripNames
  }
  return(data)
}
