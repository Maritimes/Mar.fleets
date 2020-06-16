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
               useLocal = FALSE,
               quiet=TRUE
  )

  argsSent<-  list(...)
  args[names(argsSent)] <- argsSent

  if (!do.call(canRun, args))stop("Can't run as requested.")
  if(args$useLocal){
    fleet <- do.call(get_fleet_local, list(argsList=args))
    marf <- do.call(get_MARFIS_local, list(thisFleet=fleet,argsList=args))
    obs <- do.call(get_OBS_local, list(thisFleet=fleet,get_MARFIS = marf, argsList=args))
    bycatch <- do.call(get_Bycatch_local, list(got_OBS = obs, argsList=args))
  }else{
    fleet <- do.call(get_fleet_remote, list(argsList=args))
    marf <- do.call(get_MARFIS_remote, list(thisFleet=fleet,argsList=args))
    obs <- do.call(get_OBS_remote, list(thisFleet=fleet,get_MARFIS = marf, argsList=args))
    bycatch <- do.call(get_Bycatch_remote, list(got_OBS = obs, argsList=args))
  }
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
