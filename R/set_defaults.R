# @title set_defaults
#' @description This function ensures that all possible parameters are provided.  They will be
#' overwritten by any sent directly to the sp_<species> or get_all functions.
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
  filtTrack<-new.env()
  filtTrack$mdDone <- filtTrack$gearDone <- filtTrack$nafoDone <- filtTrack$gearSpecsDone <- filtTrack$canDoGearSpecs <- filtTrack$vessLenDone <- FALSE
  argsDef <- list(filtTrack = filtTrack,
                  mdCode = "all",
                  gearCode = "all",
                  nafoCode = "all",
                  gearSpType = "all",
                  gearSpSize = "all",
                  vessLen = "all",
                  useDate = "LANDED_DATE",
                  marfSpp = NULL,
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
                  debug=FALSE
  )
  argsCheck <- names(argsDef)
  argsDef[names(args)] <- args
  dateArgs <- Mar.utils::vali_dates(dateStart = argsDef$dateStart, dateEnd = argsDef$dateEnd, year = argsDef$year)
  argsDef$dateStart <- dateArgs$dateStart
  argsDef$dateEnd <- dateArgs$dateEnd
  if (length(argsCheck) != length(argsDef)){
    jakes <- setdiff(names(argsDef),argsCheck)
    stop(paste0("COATES: This package does not understand the following parameter(s): ",paste0(jakes,collapse = ",")))
  }
  argsDef$year <- NULL

  # if (argsDef$debug==T | argsDef$quietly==F){
    # full listing of all of the parameters used
    paramDf <- argsDef
    paramDf$filtTrack <-NULL
    paramDf[lengths(paramDf)>1]<- paste0(paramDf[lengths(paramDf)>1])
    paramDf <- data.frame(PARAMETER=names(paramDf), VALUE = unlist(paramDf), row.names = NULL)
    paramDf[paramDf$PARAMETER=="dateStart","VALUE"] <- format(as.POSIXct(as.integer(paramDf[paramDf$PARAMETER=="dateStart","VALUE"]),origin = "1970-01-01"), "%Y-%m-%d")
    paramDf[paramDf$PARAMETER=="dateEnd","VALUE"] <- format(as.POSIXct(as.integer(paramDf[paramDf$PARAMETER=="dateEnd","VALUE"]),origin = "1970-01-01"), "%Y-%m-%d")
    paramDf$SOURCE <- NA
    paramDf[paramDf$PARAMETER %in% names(argsRec$argsUser),"SOURCE"] <- "user-supplied"
    paramDf[paramDf$PARAMETER %in% names(argsRec$argsFn),"SOURCE"] <- "hardcoded for this fleet"
    paramDf[is.na(paramDf$SOURCE),"SOURCE"] <- "default value (overwritable by user)"
    paramDf <- rbind(paramDf, c("filtTrack*", "<env>","<system>"))
    toMatch <- c("TRUE", "FALSE", "<env>","c\\(.*","^[0-9]*$")
    paramDf[!grepl(paste(toMatch, collapse = '|'),paramDf$VALUE),"VALUE"]<- paste0('"',paramDf[!grepl(paste(toMatch, collapse = '|'),paramDf$VALUE),"VALUE"],'"')
    paramDf <-  paramDf[with(paramDf,order(-rank(SOURCE), PARAMETER)),c( "SOURCE", "PARAMETER","VALUE")]
    cat("\n","-----------------------------------------------------------------------","\n",
        "Following is a full list of the parameters that are being used.","\n",
        "The parameters hardcoded within the species wrapper functions (e.g. sp_swordfish()) cannot be overridden.","\n",
        "For example, sp_swordfish() always uses longline, and cannot be called with a gearcode for 'traps' or 'trawls'.", "\n\n", sep = "")
    print(paramDf)
    cat('\n*\t "filtTrack" is an environment that is used to store the status of the filtering of the data','\n')
    cat("-----------------------------------------------------------------------\n")
  # }
  return(argsDef)
}