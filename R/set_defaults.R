# @title set_defaults
#' @description This function ensures that all possible parameters are provided.  They will be
#' overwritten by any sent directly to the fleet_<species> or get_all functions.
#' @family coreFuncs
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @noRd
set_defaults <- function(...){

  argsRec <- list(...)
  if (all(names(argsRec) %in% c("argsFn","argsUser"))){
    if((length(argsRec$argsUser$debug)>0) && (argsRec$argsUser$debug == TRUE)) Mar.utils::where_now(inf = as.character(sys.calls()[[sys.nframe()-1]]))
    args  <- Mar.utils::combine_lists(primary =argsRec$argsFn, ancilliary =  argsRec$argsUser, quietly=F)
  }else{
    if (argsRec$debug)  Mar.utils::where_now(as.character(sys.calls()[[sys.nframe() - 1]]))
    if((length(argsRec$debug)>0) && (argsRec$debug == TRUE)) Mar.utils::where_now(inf = as.character(sys.calls()[[sys.nframe()-1]]))
    args <- argsRec
  }

  # keep track of what filters have been applied ------------------------------------------------
  argsDef <- list(marfSpp = "all",
                  mdCode = "all",
                  gearCode = "all",
                  nafoCode = "all",
                  gearSpType = "all",
                  gearSpSize = "all",
                  vessLen = "all",
                  conditionID = "all",
                  useDate = "LANDED_DATE",
                  areas = "NAFOSubunits_sf",
                  areasField = "NAFO_1",
                  dateStart = NULL,
                  dateEnd = NULL,
                  year = NULL,
                  keepSurveyTrips = FALSE,
                  matchMarfis = TRUE,
                  data.dir = file.path(getwd(), "data"),
                  oracle.username = "_none_",
                  oracle.password = "_none_",
                  oracle.dsn = "_none_",
                  usepkg = "rodbc",
                  useLocal = FALSE,
                  quietly=TRUE,
                  debugISDBTrips = "_none_",
                  HS = FALSE,
                  debug=FALSE
  )
  # args marfTabs & isdbTabs set in can_run where they will always be run
  # (enable_local doesn't run this)
  argsDef[names(args)] <- args
  dateArgs <- Mar.utils::vali_dates(dateStart = argsDef$dateStart, dateEnd = argsDef$dateEnd, year = argsDef$year)
  argsDef$dateStart <- dateArgs$dateStart
  argsDef$dateEnd <- dateArgs$dateEnd
  argsDef$year <- NULL
  if (argsDef$areas !=  "NAFOSubunits_sf" && argsDef$areasField == "NAFO_1"){
    if (argsDef$areas == "Strata_Mar_sf") argsDef$areasField = "StrataID"
    if (argsDef$areas == "Strata_Mar_4VSW_sf") argsDef$areasField = "StrataID"

    if (argsDef$areas == "LFAs_sf") argsDef$areasField = "LFA"
    if (argsDef$areas == "Grids_Lobster_sf") argsDef$areasField = "GRID"

    if (argsDef$areas == "Areas_Snowcrab_sf") argsDef$areasField = "AREA3"
    if (argsDef$areas == "Areas_Snowcrab_Slope_sf") argsDef$areasField = "AREA2"
    if (argsDef$areas == "Areas_Shrimp_sf") argsDef$areasField = "BOX_NAME"
    if (argsDef$areas == "Areas_Surfclam_sf") argsDef$areasField = "AREA"
    if (argsDef$areas == "Areas_Halibut_sf") argsDef$areasField = "Strata"
    if (argsDef$areas == "Areas_Scallop_sf") argsDef$areasField = "StrataID"
  }


  argsCheck <- names(argsDef)

  if (length(argsCheck) != length(argsDef)){
    jakes <- setdiff(names(argsDef),argsCheck)
    stop(paste0("COATES: This package does not understand the following parameter(s): ",paste0(jakes,collapse = ",")))
  }

  # full listing of all of the parameters used
  paramDf <- argsDef
  paramDf[lengths(paramDf)>1]<- paste0(paramDf[lengths(paramDf)>1])
  paramDf <- data.frame(PARAMETER=names(paramDf), VALUE = unlist(paramDf), row.names = NULL)
  paramDf[paramDf$PARAMETER=="dateStart","VALUE"] <- format(as.POSIXct(as.integer(paramDf[paramDf$PARAMETER=="dateStart","VALUE"]),origin = "1970-01-01"), "%Y-%m-%d")
  paramDf[paramDf$PARAMETER=="dateEnd","VALUE"] <- format(as.POSIXct(as.integer(paramDf[paramDf$PARAMETER=="dateEnd","VALUE"]),origin = "1970-01-01"), "%Y-%m-%d")
  paramDf$SOURCE <- NA
  paramDf[paramDf$PARAMETER %in% names(argsRec$argsUser),"SOURCE"] <- "user-supplied"
  paramDf[paramDf$PARAMETER %in% names(argsRec$argsFn),"SOURCE"] <- "hardcoded for this fleet"
  paramDf[is.na(paramDf$SOURCE),"SOURCE"] <- "default value (overwritable by user)"
  toMatch <- c("TRUE", "FALSE","c\\(.*","^[0-9]*$")
  paramDf[!grepl(paste(toMatch, collapse = '|'),paramDf$VALUE),"VALUE"]<- paste0('"',paramDf[!grepl(paste(toMatch, collapse = '|'),paramDf$VALUE),"VALUE"],'"')
  paramDf <-  paramDf[with(paramDf,order(-rank(SOURCE), PARAMETER)),c( "SOURCE", "PARAMETER","VALUE")]
  if(!argsDef$quietly){
    cat("\n","-----------------------------------------------------------------------","\n",
        "Following is a full list of the parameters that are being used.","\n",
        "The parameters hardcoded within the species wrapper functions (e.g. fleet_swordfish()) cannot be overridden.","\n",
        "For example, fleet_swordfish() always uses longline, and cannot be called with a gearcode for 'traps' or 'trawls'.", "\n\n", sep = "")
    paramDf$VALUE<- ifelse(nchar(paramDf$VALUE)>150,"<Too long to display>",paramDf$VALUE)
    paramDf[paramDf$PARAMETER == "oracle.password","VALUE"]<- "*****"
    print(paramDf)

    cat("\n","-----------------------------------------------------------------------\n")
  }
  return(argsDef)
}
