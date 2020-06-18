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
  if (!do.call(canRun, args))stop("Can't run as requested.")

  if(!args$useLocal){
    cxn = Mar.utils::make_oracle_cxn(usepkg = args$usepkg,
                                     fn.oracle.username = args$oracle.username,
                                     fn.oracle.password = args$oracle.password,
                                     fn.oracle.dsn = args$oracle.dsn,
                                     quietly = args$quiet)
    if (!class(cxn) =="list"){
      cat("\nCan't do this without a DB connection.  Aborting.\n")
      return(NULL)
    }
    args[["cxn"]] <- cxn
  }
  fleet <- do.call(get_fleet, list(argsList=args))
  marf <- do.call(get_MARFIS, list(thisFleet=fleet,argsList=args))
  obs <- do.call(get_OBS, list(thisFleet=fleet,get_MARFIS = marf, argsList=args))
  bycatch <- do.call(get_Bycatch, list(got_OBS = obs, argsList=args))

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
