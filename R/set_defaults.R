#' @title set_defaults
#' @description This function ensures that all possible parameters are provided.  They will be
#' overwritten by any sent directly to the fleet_<species> or get_all functions.
#' @param lics default is an empty dataframe\code{'all'}
#' @param gearSpecs default is \code{'all'}
#' @param area default is \code{'all'}
#' @param marfSpp default is \code{'all'}
#' @param vessLen default is \code{'all'}
#' @param useDate default is \code{'LANDED_DATE'}
#' @param returnFleet default is \code{TRUE}
#' @param returnMARFIS default is \code{TRUE}
#' @param returnISDB default is \code{TRUE}
#' @param returnBycatch default is \code{TRUE}
#' @param returnLocations default is \code{TRUE}
#' @param useReportedNAFO default is \code{TRUE}
#' @param manual_fleet default is \code{FALSE}
#' @param areas default is \code{'NAFOSubunits_sf'}
#' @param areasField default is \code{'NAFO_1'}
#' @param dateStart default is \code{NULL}
#' @param dateEnd default is \code{NULL}
#' @param year default is \code{NULL}
#' @param keepSurveyTrips default is \code{FALSE}
#' @param matchMarfis default is \code{TRUE}
#' @param matchMaxDayDiff default is \code{15}
#' @param dropUnmatchedISDB default is \code{TRUE}
#' @param data.dir default is \code{'file.path(getwd(), "data")'}
#' @param oracle.username default is \code{'_none_'}
#' @param oracle.password default is \code{'_none_'}
#' @param oracle.dsn default is \code{'_none_'}
#' @param usepkg default is \code{'rodbc'}
#' @param useLocal default is \code{FALSE}
#' @param quietly default is \code{TRUE}
#' @param debugISDBTrips default is \code{'_none_'}
#' @param HS default is \code{FALSE}
#' @param debug default is \code{FALSE}
#' @param ... other arguments passed to methods
#' @family coreFuncs
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
set_defaults <- function(lics = 'all',
                         gearSpecs = 'all',
                         area = 'all',
                         marfSpp = "all",
                         vessLen = "all",
                         useDate = "LANDED_DATE",
                         returnFleet = T,
                         returnMARFIS = T,
                         returnISDB = T,
                         returnBycatch = T,
                         returnLocations = T,
                         useReportedNAFO = TRUE,
                         manual_fleet=F,
                         areas = "NAFOSubunits_sf",
                         areasField = "NAFO_1",
                         dateStart = NULL,
                         dateEnd = NULL,
                         year = NULL,
                         keepSurveyTrips = FALSE,
                         matchMarfis = TRUE,
                         matchMaxDayDiff = 15,
                         dropUnmatchedISDB = TRUE,
                         data.dir = file.path(getwd(), "data"),
                         oracle.username = "_none_",
                         oracle.password = "_none_",
                         oracle.dsn = "_none_",
                         usepkg = "rodbc",
                         useLocal = FALSE,
                         quietly=TRUE,
                         debugISDBTrips = "_none_",
                         debugLics = NULL,
                         HS = FALSE,
                         debuggit=FALSE,
                         ...){
  defaults <- as.list(environment())
  sentArgs <- list(...)

  #ensure hardcoded args take priority over user args
  submittedArgs <- Mar.utils::combine_lists(primary = sentArgs$argsFn, ancilliary = sentArgs$argsUser, quietly = T)

  #ensure submitted args take priority over default args
  argg <- Mar.utils::combine_lists(primary =  submittedArgs, ancilliary = defaults, quietly = T)

  # have all of our arguments - further process some of them ------------------------------------------------------------------------------------------------
  # convert year (if present to dateStart and dateEnd)
  dateArgs <- Mar.utils::vali_dates(dateStart = argg$dateStart, dateEnd = argg$dateEnd, year = argg$year, quietly = argg$quietly)
  argg$dateStart <- dateArgs$dateStart
  argg$dateEnd <- dateArgs$dateEnd
  argg$year <- NULL

  #set the field to use for non-NAFO
  if (argg$areas !=  "NAFOSubunits_sf" && argg$areasField == "NAFO_1"){
    if (argg$areas == "Strata_Mar_sf") argg$areasField = "StrataID"
    if (argg$areas == "Strata_Mar_4VSW_sf") argg$areasField = "StrataID"

    if (argg$areas == "LFAs_sf") argg$areasField = "LFA"
    if (argg$areas == "Grids_Lobster_sf") argg$areasField = "GRID"

    if (argg$areas == "Areas_Snowcrab_sf") argg$areasField = "AREA3"
    if (argg$areas == "Areas_Snowcrab_Slope_sf") argg$areasField = "AREA2"
    if (argg$areas == "Areas_Shrimp_sf") argg$areasField = "BOX_NAME"
    if (argg$areas == "Areas_Surfclam_sf") argg$areasField = "AREA"
    if (argg$areas == "Areas_Halibut_sf") argg$areasField = "Strata"
    if (argg$areas == "Areas_Scallop_sf") argg$areasField = "StrataID"
  }

  # notify user on unknown sent parameters
  jakes <- setdiff(names(argg),names(defaults))
  if (length(jakes)>0){
    warning(paste0("This package does not understand the following parameter(s): ",paste0(jakes,collapse = ",")))
  }

  if(!argg$quietly){
    #create a table outlining info about params
    paramDf <- argg
    paramDf[lengths(paramDf)>1]<- paste0(paramDf[lengths(paramDf)>1])
    paramDf <- data.frame(PARAMETER=names(paramDf), VALUE = unlist(paramDf), row.names = NULL)
    paramDf[paramDf$PARAMETER=="dateStart","VALUE"] <- format(as.Date(argg$dateStart, origin = "1970-01-01"), "%Y-%m-%d")
    paramDf[paramDf$PARAMETER=="dateEnd","VALUE"] <- format(as.Date(argg$dateEnd, origin = "1970-01-01"), "%Y-%m-%d")
    paramDf$SOURCE <- NA

    paramDf[is.na(paramDf$SOURCE),"SOURCE"] <- "default value (overwritable by user)"
    paramDf[paramDf$PARAMETER %in% names(sentArgs$argsUser),"SOURCE"] <- "user-supplied"
    paramDf[paramDf$PARAMETER %in% names(sentArgs$argsFn),"SOURCE"] <- "hardcoded for this fleet"
    toMatch <- c("TRUE", "FALSE","c\\(.*","^[0-9]*$")
    paramDf[!grepl(paste(toMatch, collapse = '|'),paramDf$VALUE),"VALUE"]<- paste0('"',paramDf[!grepl(paste(toMatch, collapse = '|'),paramDf$VALUE),"VALUE"],'"')
    paramDf <-  paramDf[with(paramDf,order(-rank(SOURCE), PARAMETER)),c( "SOURCE", "PARAMETER","VALUE")]
    cat("\n","-----------------------------------------------------------------------","\n",
        "Following is a full list of the parameters that are being used.","\n",
        "The parameters hardcoded within the species wrapper functions (e.g. fleet_swordfish()) cannot be overridden.","\n",
        "For example, fleet_swordfish() always uses longline, and cannot be called with a gearcode for 'traps' or 'trawls'.", "\n\n", sep = "")
    paramDf$VALUE<- ifelse(nchar(paramDf$VALUE)>150,"<Too long to display>",paramDf$VALUE)
    paramDf[paramDf$PARAMETER == "oracle.password","VALUE"]<- "*****"
    print(paramDf)

    cat("\n","-----------------------------------------------------------------------\n")
  }
  return(argg)
}
