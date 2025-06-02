#' @title set_defaults
#' @description This function provides all of the possible parameters understood by this package.
#' Some can be overwritten by \code{fleet_*} wrappers.
#' @details The desired date range can be specified multiple ways - either using \code{year} (alone),
#' \code{dateStart} (alone) or \code{dateStart} with \code{dateEnd} (explained below).
#' @param year default is \code{NULL}.  \code{year} can be used if data should be extracted for an
#' entire calendar year (i.e. Jan 1 --> Dec 31).  The format is
#' \code{YYYY}, e.g. \code{year = 2018}.  \code{dateStart} takes priority over \code{year} if both
#' are provided.
#' @param dateStart default is \code{NULL}.  This identifies the start date of the search window, in
#' the format \code{YYYY-MM-DD} (e.g. \code{dateStart = "2019-02-19"}).
#' If no \code{dateEnd} is provided, the window will be for 1 year (i.e, 365 days).  ,
#' @param dateEnd default is \code{NULL} format is \code{YYYY-MM-DD}, e.g. \code{dateEnd = "2019-02-19"}.
#' \code{dateEnd} must be associated with a valid entry of
#' \code{dateStart} to identify a block of time for a data extraction  (e.g. Jun18, 2018 -->
#' August 27, 2019).
#' @param lics default is \code{'all'}.  Fleet wrappers will populate this with a dataframe specifying
#' the licence types, subtypes, gear and licence species valid
#' for a particular fleet.  This information comes from  \code{Mar.fleets::LIC_CORE}.
#' @param gearSpecs default is \code{'all'}. Fleet wrappers may populate it with a dataframe
#' specifying the particular specifications for a fleets' gear.
#' For example, certain fleets must use particular mesh sizes or shapes. This information comes from
#' \code{Mar.fleets::LIC_GEAR_SPECS}.
#' @param area default is \code{'all'}.  Certain fleets are only licenced to fish in certain areas.
#' This information comes from \code{Mar.fleets::licAreas}.
#' @param marfSpp default is \code{'all'}. The marfis species code, usually sent by the fleet
#' wrapper.
#' @param marfGear default is \code{'all'}, but all wrappers have (overwritable) fleet-specific
#' values.  This is a vector of MARFIS gear codes known to have caught this species.
#' @param isdbSpp default is \code{'all'}. The ISDB species code, usually sent by the fleet wrapper
#' @param tripcd_id default is \code{NULL}.  If a tripcd_id from ISDB is provided, all matting
#' records will be examined for matches
#' @param returnMARFIS default is \code{TRUE}. Do you want a list object containing marfis trip and
#' set information as part of your results?
#' @param returnISDB default is \code{TRUE}. Do you want a list object containing isdb trip and set
#' information as part of your results? (requires \code{returnMARFIS = T})
#' @param areaFile default is \code{'NAFOSubunits_sf'}.  This is used to identify which areas to
#' check the trips and sets against. By default,
#' Mar.data::NAFOSubunits_sf is ued, but any objects in Mar.data could be used.
#' @param areaFileField default is \code{'NAFO_1'}. This is a field within the \code{areas} object
#' which specifies exactly which field of the areas object data
#' should be compared against.
#' @param nafoDet default is \code{2}, but values between \code{1} and \code{4} are acceptable. This
#' specifies the level of detail that will be used in the summarized locations table.  Using the
#' default value of 2, trips and sets will be summarized by areas such as "4X", "4V" and "5Z" (i.e 2
#' characters).  If set to "1", areas would be more general  (e.g. "3", "4", "5"; i.e. 1 character),
#' while a value like 4 would summarize the trips and sets into very specific NAFO subunits (e.g.
#' "3PSA","4VSB" and "5ZEM")
#' @param keepSurveyTrips default is \code{TRUE}. Within the ISDB database are non-commercial,
#' survey trips.  Setting this to \code{FALSE} ensures these trips are dropped.
#' @param keepMissingGear default is \code{TRUE}. Some fleets have particular allowable gear sizes
#' and types (see gearSpecs). Many cases exist where all of the gear details are not filled it.
#' When this parameter is set to \code{TRUE}, these 'unknown' types and sizes are retained, and the
#' values are set to -999.  If it is set to \code{FALSE}, any gears with missing values are dropped
#' - and they are not included in the results.
#' @param maxTripDiff_Hr default is \code{48}. Any MARFIS and ISDB trips that vary by more than the
#' # of days specified here will NOT be considered matches (on the basis of common Vessel, licence
#' and date).  They may still match on confirmation codes and/or trip names.
#' @param maxSetDiff_Hr default is \code{48}. Any MARFIS and ISDB sets that vary by more than the
#' # of hours specified here will NOT be considered matches.
#' @param maxSetDiff_Km default is \code{100}. Any MARFIS and ISDB sets with positions more than the
#' # of kilometers specified here will NOT be considered matches.
#' @param dropUnmatchedISDB default is \code{TRUE}.
#' @param manualMatch default is \code{FALSE}. This parameter is only used when calling functions
#' from \code{manual_matcher()}.  It ensures that the functions work properly with its reduced
#' input format.
#' @param socks default is \code{FALSE}. Normally, un-QC'd wrappers generate a prompt forcing the
#' user to acknowledge that they are aware that the script needs further testing.  Setting this to
#' TRUE will prevent the prompt from showing up. Why 'socks'?  You had to be there.
#' @param useLocal default is \code{FALSE}. This specifies whether to run the script against local
#' data or against Oracle (requires network or VPN).
#' Depending on your value for \code{useLocal}, different values become necessary.
#' \itemize{
#'  \item{useLocal=TRUE} This implies that you have local data you want to use.
#'  \item{useLocal=FALSE} This implies that you have will query Oracle for the necessary data.
#'  Ensure you include:
#'   \itemize{
#'       \item param \code{cxn} a valid oracle connection (e.g. an roracle, rodbc or dbi connection object)
#'     }
#' }
#' @param debug default is \code{FALSE}. If TRUE, this parameter causes the package to run in
#' debug mode, providing much extraneous information.
#' @param debugLics default is \code{NULL}.  If a vector of LICENCE_IDs is provided, the script will
#' provide information about when the script drops them from
#' consideration.
#' @param debugVRs default is \code{NULL}.  If a vector of VR numbers is provided, the script will
#' provide information about when the script drops them from
#' consideration.
#' @param debugMARFTripIDs default is \code{NULL}.  If a vector of MARFIS trip IDs is provided, the
#' script will provide information about when the script drops them from
#' consideration.
#' @param debugISDBTripIDs default is \code{NULL}.  If a vector of ISDB trip IDs is provided, the
#' script will provide information about when the script drops them from
#' consideration.
#' @param debugISDBTripNames default is \code{NULL}.  If a vector of ISDB trip names is provided,
#' the script will provide information about when the script drops them from
#' consideration. Trip "names" are typically in a format like "J18-1234" or "A18-1234A".
#' @param ... other arguments passed to methods
#' @family coreFuncs
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
set_defaults <- function(lics = 'all',
                         gearSpecs = 'all',
                         area = 'all',
                         marfGear = 'all',
                         marfSpp = 'all',
                         isdbSpp = 'all',
                         tripcd_id = NULL,
                         returnMARFIS = T,
                         returnISDB = T,
                         areaFile = 'NAFOSubunits_sf',
                         areaFileField = 'NAFO_1',
                         nafoDet = 2,
                         dateStart = NULL,
                         dateEnd = NULL,
                         year = NULL,
                         keepSurveyTrips = TRUE,
                         keepMissingGear = TRUE,
                         maxTripDiff_Hr = 48,
                         maxSetDiff_Hr = 48,
                         maxSetDiff_Km = 100,
                         dropUnmatchedISDB = TRUE,
                         manualMatch = FALSE,
                         socks =FALSE,
                         data.dir = get_pesd_fl_dir(),
                         cxn = NULL,
                         thecmd = NULL,
                         useLocal = FALSE,
                         debugLics = NULL,
                         debugVRs = NULL,
                         debugMARFTripIDs = NULL,
                         debugISDBTripIDs = NULL,
                         debugISDBTripNames = NULL,
                         debug=FALSE,
                         ...){
  defaults <- as.list(environment())
  sentArgs <- list(...)
  #ensure hardcoded args take priority over user args
  submittedArgs <- Mar.utils::combine_lists(primary = sentArgs$argsFn, ancilliary = sentArgs$argsUser, quietly = T)
  #ensure submitted args take priority over default args
  argg <- Mar.utils::combine_lists(primary =  submittedArgs, ancilliary = defaults, quietly = T)
  if (argg$debug)    t27 <- Mar.utils::where_now(returnTime = T)
  # have all of our arguments - further process some of them ------------------------------------------------------------------------------------------------

  argg$thecmd = Mar.utils::connectionCheck(argg$cxn)
  # convert year (if present to dateStart and dateEnd)
  dateArgs <- Mar.utils::vali_dates(dateStart = argg$dateStart, dateEnd = argg$dateEnd, year = argg$year, quietly = T)
  argg$dateStart <- dateArgs$dateStart
  argg$dateEnd <- dateArgs$dateEnd
  argg$year <- NULL
  #set the field to use for non-NAFO
  if (argg$areaFile !=  'NAFOSubunits_sf' && argg$areaFileField == 'NAFO_1'){
    if (argg$areaFile == 'Strata_Mar_sf') argg$areaFileField = 'StrataID'
    if (argg$areaFile == 'Strata_Mar_4VSW_sf') argg$areaFileField = 'StrataID'
    if (argg$areaFile == 'LFAs_sf') argg$areaFileField = 'LFA'
    if (argg$areaFile == 'Grids_Lobster_sf') argg$areaFileField = 'GRID'
    if (argg$areaFile == 'Areas_Snowcrab_sf') argg$areaFileField = 'AREA1'
    if (argg$areaFile == 'Areas_Snowcrab_Slope_sf') argg$areaFileField = 'AREA2'
    if (argg$areaFile == 'Areas_Shrimp_sf') argg$areaFileField = 'BOX_NAME'
    if (argg$areaFile == 'Areas_Surfclam_sf') argg$areaFileField = 'AREA'
    if (argg$areaFile == 'Areas_Halibut_sf') argg$areaFileField = 'Strata'
    if (argg$areaFile == 'Areas_Scallop_sf') argg$areaFileField = 'StrataID'
    if (argg$areaFile == 'SPAs_Scallop_sf') argg$areaFileField = 'layer'
  }

  # notify user on unknown sent parameters
  jakes <- setdiff(names(argg),names(defaults))
  if ("quietly" %in% jakes) message("The 'quietly' parameter is no longer valid")
  jakes = jakes[!(jakes %in% 'quietly')]

  if (length(jakes)>0){
    warning(paste0("This package does not understand the following parameter(s): ",paste0(jakes,collapse = ",")))
  }
  paramDf <- argg
  paramDf <- replace(paramDf, sapply(paramDf, is.data.frame), "see <results>$params$fleet$...")
  paramDf[lengths(paramDf)>1]<- paste0(paramDf[lengths(paramDf)>1])
  paramDf <- replace(paramDf, sapply(paramDf, is.null), "<NULL>")
  paramDf <- paramDf[!names(paramDf) %in%  c("cxn", "thecmd")]

  paramDf <- data.frame(PARAMETER=names(paramDf), VALUE = unlist(paramDf), row.names = NULL)
  paramDf[paramDf$PARAMETER=="dateStart","VALUE"] <- format(as.Date(argg$dateStart, origin = "1970-01-01"), "%Y-%m-%d")
  paramDf[paramDf$PARAMETER=="dateEnd","VALUE"] <- format(as.Date(argg$dateEnd, origin = "1970-01-01"), "%Y-%m-%d")
  paramDf$SOURCE <- NA

  paramDf[is.na(paramDf$SOURCE),"SOURCE"] <- "default value (overwritable by user)"
  paramDf[paramDf$PARAMETER %in% names(sentArgs$argsUser),"SOURCE"] <- "user-supplied"
  paramDf[paramDf$PARAMETER %in% names(sentArgs$argsFn),"SOURCE"] <- "hardcoded for this fleet"

  if("year" %in% names(sentArgs$argsUser)){
    paramDf[paramDf$PARAMETER == "dateStart","SOURCE"] <- "derived from user-supplied 'year'"
    paramDf[paramDf$PARAMETER == "dateEnd","SOURCE"] <- "derived from user-supplied 'year'"
  }

  toMatch <- c("TRUE", "FALSE","c\\(.*","^[0-9]*$")
  paramDf[!grepl(paste(toMatch, collapse = '|'),paramDf$VALUE),"VALUE"]<- paste0('"',paramDf[!grepl(paste(toMatch, collapse = '|'),paramDf$VALUE),"VALUE"],'"')
  paramDf <-  paramDf[with(paramDf,order(-rank(SOURCE), PARAMETER)),c( "SOURCE", "PARAMETER","VALUE")]
  paramDf$VALUE<- ifelse(nchar(paramDf$VALUE)>150,"<Too long to display>",paramDf$VALUE)
  paramDf <- rbind(paramDf, c("metadata","Date Run", format(Sys.Date(), "%Y-%m-%d")))
  if(all(is.na(utils::packageDescription("Mar.fleets")))){
    paramDf <- rbind(paramDf, c("metadata","Mar.fleets not installed"))
  }else{
    paramDf <- rbind(paramDf, c("metadata","Mar.fleets version", utils::packageDescription("Mar.fleets")$Version))
  }
  # paramDf <- rbind(paramDf, c("metadata","Mar.fleets version", utils::packageDescription("Mar.fleets")$Version))
  if(exists("dbEnv")){
    dbEnv$debugLics <- argg$debugLics
    dbEnv$debugVRs <- argg$debugVRs
    dbEnv$debugMARFTripIDs <- argg$debugMARFTripIDs
    dbEnv$debugISDBTripIDs <- argg$debugISDBTripIDs
    if (!is.null(argg$debugISDBTripNames)) {
      dbEnv$debugISDBTripNamesLookup <- clean_ISDB_Trip(df=data.frame(ISDB_TRIP = argg$debugISDBTripNames), field = "ISDB_TRIP", out_name = "ISDB_TRIP_CLN")
      dbEnv$debugISDBTripNames <- dbEnv$debugISDBTripNamesLookup$ISDB_TRIP_CLN
    }
  }
  res <- list()
  res[["params"]]<- paramDf
  res[["args"]]<- argg

  if (argg$debug){
    t27_ <- proc.time() - t27
    message("\tExiting set_defaults() (",round(t27_[1],0),"s elapsed)")
  }
  return(res)
}
