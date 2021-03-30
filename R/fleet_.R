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
  if (argsUser$debuggit){
    Mar.utils::where_now()
    print(lics)
    print(area)
    print(gearSpecs)
  }
  # add remaining default args ------------------------------------------------------------------------------------------------------------------------------
  args <- do.call(set_defaults, list(argsFn=argsFn, argsUser=argsUser))

  # Verify we have necessary data/permissions ---------------------------------------------------------------------------------------------------------------
  args <- do.call(can_run, args)
  #set up results list, and populate according to arguments
  data <- list()
  if (args$returnFleet){
    fleet <- do.call(get_fleet, args)
    data[["fleet"]]<- fleet
  }
  if (args$returnMARFIS){
    marf <- do.call(get_marfis, list(thisFleet=fleet$FLEET_ACTIVITY,args=args))
    data[["marf"]]<- marf
  }

  if (args$returnISDB){
    isdb <- do.call(get_isdb, list(thisFleet=fleet$FLEET_ACTIVITY,get_marfis = marf, matchMarfis = T, args=args))

    if(args$dropUnmatchedISDB){
      isdb$ISDB_TRIPS <- isdb$ISDB_TRIPS[!is.na(isdb$ISDB_TRIPS$TRIP_ID_MARF),]
      isdb$ISDB_SETS <- isdb$ISDB_SETS[!is.na(isdb$ISDB_SETS$TRIP_ID_MARF),]
    }

    if (length(isdb)>1 && class(isdb$ISDB_TRIPS)=="data.frame"){
      data[["isdb"]]<- isdb

      if (args$returnBycatch){
        bycatch <- do.call(get_bycatch, list(isTrips = unique(isdb$ISDB_TRIPS[!is.na(isdb$ISDB_TRIPS$TRIP_ID_MARF), "TRIP_ID_ISDB"]), args=args))
        data[["bycatch"]]<- bycatch
      }

      if (args$returnLocations){
        loc <- do.call(summarize_locations, list(get_isdb = isdb, get_marfis = marf, args=args))
        data[["location_sumary"]]<- loc
      }
    }
  }


return(data)
}
