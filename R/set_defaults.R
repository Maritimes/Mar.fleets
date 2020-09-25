# @title set_defaults
#' @description This function ensures that all possible parameters are provided.  They will be
#' overwritten by any sent directly to the sp_<species> or get_all functions.
#' @family coreFuncs
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @noRd
set_defaults <- function(...){

  # keep track of what filters have been applied ------------------------------------------------
  filtTrack<-new.env()
  filtTrack$mdDone <- filtTrack$gearDone <- filtTrack$nafoDone <- filtTrack$gearSpecsDone <- filtTrack$canDoGearSpecs <- filtTrack$vessLenDone <- FALSE
  args <- list(mdCode = "all",
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
               filtTrack = filtTrack,
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
  if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
  argsSent <- list(...)
  args[names(argsSent)] <- argsSent
  dateArgs <- vali_dates(dateStart = args$dateStart, dateEnd = args$dateEnd, year = args$year)
  args$dateStart <- dateArgs$dateStart
  args$dateEnd <- dateArgs$dateEnd
  if (args$debug) print(args)
  return(args)
}
