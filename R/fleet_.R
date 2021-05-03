#' @title fleet_
#' @description This function is a generic wrapper that facilitates extracting fleet specific information using the data provided by fleet-specific wrappers.
#' @param fleet This is an identifier that will be used to interrogate the licCore object to select the correct type(s), subtype(s), gear(s), and licence_species for the selected fleet
#' @param area This is an identifier that will be used to interrogate the licAreas object to select any NAFO areas associated with the selected fleet
#' @param gearSpecs This is an identifier that will be used to interrogate the licGearSpecs object to select any gear parameters associated with the selected fleet
#' @inheritDotParams set_defaults
#' @examples \dontrun{
#' test <- fleet_(fleet="POLLOCK_MOBILE", area="WESTERN", gearSpecs="SMALL" )
#'  }
#' @family coreFuncs
#' @return list of objects, including marfis data, isdb data (including all catches), and
#' information for matching isdb and marfis data
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
    fleet <- do.call(get_fleet, args)
    data[["fleet"]]<- fleet
  data$fleet$debug <-NULL

  if (args$returnMARFIS){
    marf <- do.call(get_marfis, list(thisFleet=fleet$FLEET_ACTIVITY,args=args))
    data[["marf"]]<- marf

    data$marf$debug <-NULL
    if (args$returnISDB){
      isdb <- do.call(get_isdb, list(thisFleet=fleet$FLEET_ACTIVITY,get_marfis = marf, matchMarfis = T, args=args))

      if(args$dropUnmatchedISDB){
        isdb$ISDB_TRIPS <- isdb$ISDB_TRIPS[!is.na(isdb$ISDB_TRIPS$TRIP_ID_MARF),]
        isdb$ISDB_SETS <- isdb$ISDB_SETS[!is.na(isdb$ISDB_SETS$TRIP_ID_MARF),]
      }

      if (length(isdb)>1 && class(isdb$ISDB_TRIPS)=="data.frame"){
        data[["isdb"]]<- isdb
        data$isdb$debug <-NULL
        loc <- do.call(summarize_locations, list(get_isdb = isdb, get_marfis = marf, args=args))
        data[["location_summary"]]<- loc
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
  return(data)
}
