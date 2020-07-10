#' @title get_all
#' @description This function serves as a wrapper for calling \code{get_fleet(...)},\code{get_marfis(...)}
#' and \code{get_bycatch(...)} individually.
#' @param ... other arguments passed to methods
#' @return returns a list including the fleet, and the resultant information from MARFIS, OBS, and the bycatch
#' @note This function can accept many parameters.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_all <- function(...){
  keep<-new.env()
  keep$mdDone <- keep$gearDone <- keep$nafoDone <- keep$gearSpecsDone <- keep$canDoGearSpecs <- keep$vessLenDone <- FALSE
  args <- list(mdCode = "all",
               gearCode = "all",
               nafoCode = "all",
               gearSpType = "all",
               gearSpSize = "all",
               vessLen = "all",
               useDate = "landed",
               keep = keep,
               keepSurveyTrips = FALSE,
               data.dir = file.path(getwd(), "data"),
               oracle.username = "_none_",
               oracle.password = "_none_",
               oracle.dsn = "_none_",
               usepkg = "rodbc",
               useLocal = FALSE,
               quiet=TRUE,
               debug=FALSE
  )

  argsSent<-  list(...)
  args[names(argsSent)] <- argsSent
  args[["theDate"]] <- ifelse(args$useDate =="fished","DATE_FISHED", "LANDED_DATE")
  if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
  cxnCheck <- do.call(can_run, args)
  if (!(is.list(cxnCheck) || cxnCheck==TRUE)){
    stop("Can't run as requested.")
  } else if (is.list(cxnCheck)){
    args[["cxn"]] <- cxnCheck
  }
  fleet <- do.call(get_fleet, list(argsList=args))
  marf <- do.call(get_marfis, list(thisFleet=fleet,argsList=args))
  obs <- do.call(get_obs, list(thisFleet=fleet,get_marfis = marf, argsList=args))
  bycatch <- do.call(get_bycatch, list(get_obs = obs, argsList=args))
  # Capture the results in a list and return them ------------------------------------------------
  cat("\nTot MARF catch: ",sum(marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000)
  cat("\nTot MARF ntrips: ",length(unique(marf$MARF_TRIPS$TRIP_ID_MARF)))
  cat("\n")
  res=list()
  res[["fleet"]]<- fleet
  res[["marf"]]<- marf
  res[["obs"]]<- obs
  res[["bycatch"]]<- bycatch
  return(res)
}
