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
               dateStart = as.Date(paste0(as.numeric(format(Sys.Date(), "%Y")) - 1,"-01-01")),
               dateEnd = NULL,
               filtTrack = filtTrack,
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
  if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
  argsSent <- list(...)$argsList

  # #seems that T or F when added to a list, do not become a TRUE or FALSE.  Force it in case that's
  # #what people send.
  # lnames<-names(argsSent)
  # argsSent <- lapply(1:length(argsSent), function(x) {
  #   res <- argsSent[[x]]
  #   if (length(res)==1 && res =="T") {
  #     this1<-TRUE
  #   } else if (length(res)==1 && res =="F"){
  #     this1<-FALSE
  #   } else{
  #     this1=res
  #   }
  # })
  # names(argsSent)<-lnames

  args[names(argsSent)] <- argsSent

  # validate sent dates, and convert year -------------------------------------------------------
  if(!is.null(args$year)){
    args$dateStart<-args$year
    args$year<-NULL
  }
  if(!is.null(args$dateStart)){
    if (nchar(args$dateStart)==4){
      dateStart <- try( as.Date( paste0(as.character(args$dateStart),"-01-01"), format= "%Y-%m-%d", origin="1970-01-01" ) )
    }else{
      dateStart <- try( as.Date( as.character(args$dateStart), format= "%Y-%m-%d", origin="1970-01-01" ) )
    }
    if( class( dateStart ) == "try-error" || is.na( dateStart ) ){
      stop("\n","The value for dateStart was not a valid year (YYYY) or date (YYYY-MM-DD)")
    }else{
      args$dateStart <- as.POSIXct(dateStart, origin = "1970-01-01")
    }
  }
  if(!is.null(args$dateEnd)){
    if (nchar(args$dateEnd)==4){
      dateEnd <- try( as.Date( paste0(as.character(args$dateEnd),"-01-01"), format= "%Y-%m-%d", origin="1970-01-01" ) )
    }else{
      dateEnd <- try( as.Date( as.character(args$dateEnd), format= "%Y-%m-%d", origin="1970-01-01" ) )
    }
    if( class( dateEnd ) == "try-error" || is.na( dateEnd ) ){
      stop("\n","The value for dateEnd was not a valid year (YYYY) or date (YYYY-MM-DD)")
    }else{
      args$dateEnd <- as.POSIXct(dateEnd, origin = "1970-01-01")
    }
  }else{
    args$dateEnd <- as.POSIXct(format(as.Date(dateStart) + 365, "%Y-%m-%d"), origin = "1970-01-01")
  }
  return(args)
}
