#' @title clean_ISDB_Trip
#' @description This function cleans ISDB trip names of spaces/odd characters to improve the potential for matching.
#' @param df default is \code{NULL}. This is a dataframe that includes a column of raw ISDB trip names
#' @param field default is \code{"ISDB_TRIP"}. This is the name of the field that contains the raw ISDB trip names
#' @param out_name default is \code{'ISDB_TRIP_CLN'}. The is the name of the new field that will be created i \code{df} after cleaning
#' @family utils
#' @return returns a list with 2 dataframes - "trips", and "sets".
#' "trips" contains all of the information necessary for identifying a trip
#' within MARFIS, as well as associated information about the trip
#' from the HAIL_OUTS and HAIL_IN_CALLS tables (e.g. confirmation numbers).
#' "sets" contains information about individual fishing activities, including
#' locations, dates, durations, gear amount, etc..
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
clean_ISDB_Trip <- function(df=NULL, field = "ISDB_TRIP", out_name="ISDB_TRIP_CLN"){
  df[,out_name] <- gsub(pattern = "[^[:alnum:]]", replacement = "", x=  df[,field])
  return(df)
}

#' @title valuesOK
#' @description This function ensures that the user has not tried to send unacceptable values
#' @param valSent default is \code{NULL}. This is a vector of values sent by the user
#' @param valOK default is \code{NULL}. This is a vector of of known, acceptable values
#' @param valID default is \code{"this vector"}. This is an identifier for the vector.
#' @family utils
#' @return returns NULL
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
valuesOK <- function(valSent = NULL, valOK = NULL, valID = "this vector"){
  if (is.null(valSent)) stop(valID, " cannot be NULL")
  if (any(!(valSent) %in% valOK)) stop(paste0("Please limit specified values of '",valID,"' to any/all of the default values."))
}

#' @title isDraft
#' @description This function ensures that users are informed that the analytic is draft, and will
#' only run with their acknowledgment
#' @family utils
#' @return returns NULL
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
isDraft <- function(){
  this <- utils::winDialog(type = c("okcancel"), 'By proceeding, I acknowlege that this analytic is in draft form, and that the asessment lead has not reviewed its results')
  message("You are welcome to contact Mike.McMahon@dfo-mpo.gc.ca, and ask that he work with the assessment lead and prioritize QC'ing this analytic.  Alternatively, if you ARE the assessment lead, please ensure you provide feedback so we can work towards removing this message.")
  if (this != "OK") stop("Stopped by user")
}

#' @title paramOK
#' @description This function looks for issues with sent parameters.
#' @param useLocal default is \code{NULL}. This specifies whether to run the script against local data or against Oracle (requires network or VPN).
#' @param p default is \code{NULL}. This is a list of the parameters sent by the user
#' @family utils
#' @return returns TRUE or FALSe, and informative messages
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
paramOK <-function(useLocal = NULL, p = NULL){
  res <- TRUE
  msgs <- NULL
  if (useLocal) {
    if (!("data.dir" %in% names(p))) {
      msgs<- c(msgs, "'data.dir' must be provided if useLocal = TRUE")
      res <- FALSE
    }
  }else{
    if (!("oracle.username" %in% names(p))) msgs <- c(msgs, "If you provide 'oracle.username =<yourname>' in the function call, you will not be prompted for it")
    if (!("oracle.password" %in% names(p))) msgs <- c(msgs, "If you provide 'oracle.password =<yourpassword>' in the function call, you will not be prompted for it")
  }

  if (!any(c("year","dateStart") %in% names(p))) {
    msgs<- c(msgs, "Either 'year' (YYYY) or 'dateStart' ('YYYY-MM-DD') must be passed to this function. \n\t('dateEnd' ('YYYY-MM-DD') may also be passed in conjunction with 'dateStart')")
    res <- FALSE
  }

  for (i in 1:length(msgs)){
    message(msgs[i])
  }
  return(res)
}

#' @title go
#' @description This function prevents people from running the package until it's ready
#' @family utils
#' @return returns TRUE or FALSe, and informative messages
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
go <- function(){
  res <- FALSE
  inf <- as.list(Sys.info())
  if (tolower(inf$login) %in% c("mcmahonm","bowlbyh"))res <- TRUE
  return(res)
}

#' @title can_run
#' @description This function assesses the whether or not the package can accomodate a request to
#' run locally or directly against  oracle.  If the desire is to run locally, all of the necessary
#' tables must be available in the data.dir.  If run against Oracle, we must be on the network and
#' have access to the necessary schema.tables.
#' @family setup
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @noRd
can_run <- function(...){
  args=list(...)
  if (args$debug) t28 <- Mar.utils::where_now(returnTime = T)

  connect_Oracle <-function(...){
    #connect_Oracle is just results of trying to establish connection
    args=list(...)$args
    if (args$debug) t29 <- Mar.utils::where_now(returnTime = T)
    cxn <- do.call(Mar.utils::make_oracle_cxn, list(usepkg = args$usepkg,
                                                    fn.oracle.username = args$oracle.username,
                                                    fn.oracle.password = args$oracle.password,
                                                    fn.oracle.dsn = args$oracle.dsn,
                                                    quietly = TRUE))
    if (args$debug) {
      t29_ <- proc.time() - t29
      message("\tExiting connect_Oracle() (",round(t29_[1],0),"s elapsed)")
    }
    return(cxn)
  }

  tblAccess <- function(tables = NULL,...){
    #tblAccess is T if has necess permiss
    args=list(...)$args
    if (args$debug) t30 <- Mar.utils::where_now(returnTime = T)
    tables <- gsub("ISDB","OBSERVER", tables)
    fails = 0
    for (t in 1:length(tables)){
      message(paste0("Checking access to ",tables[t],": "))
      qry = paste0("select '1' from ", tables[t], " WHERE ROWNUM<=1")
      test = args$cxn$thecmd(args$cxn$channel, qry, rows_at_time = 1)
      if (is.character(test)) {
        fails=fails+1
        # if (quietly=FALSE)message(" failed","\n")
      }else{
        # if (quietly=FALSE)message(" success","\n")
      }
    }
    if (fails>0){
        if (args$debug){
          t30_ <- proc.time() - t30
        message("\tExiting tblAccess() (",round(t30_[1],0),"s elapsed) - missing access to some")
      }
      return(FALSE)
    }else{
      if (args$debug){
        t30_ <- proc.time() - t30
        message("\tExiting tblAccess() (",round(t30_[1],0),"s elapsed)")
      }
        return(TRUE)
    }
  }

  wantLocal <- function(tables = NULL, ...){
    #wantLocal is T if all necessar things are found
    args<-list(...)$args
    if (args$debug) t31 <- Mar.utils::where_now(returnTime = T)
    #1 check if local copies available '
    tabs = paste0(args$data.dir,.Platform$file.sep,tables,".RData")
    localDataCheck <- all(sapply(X =tabs, file.exists))
    rm(tabs)
    # }
    if (args$debug){
      t31_ <- proc.time() - t31
      message("\tExiting wantLocal() (",round(t31_[1],0),"s elapsed)")
    }
    return(localDataCheck)
  }

  marfTabs = c("HAIL_IN_CALLS",
               "HAIL_OUTS",
               "LOG_EFRT_ENTRD_DETS",
               "LOG_EFRT_STD_INFO",
               "MARFLEETS_LIC",
               "MON_DOC_ENTRD_DETS",
               "NAFO_UNIT_AREAS",
               "PRO_SPC_INFO",
               "TRIPS",
               "VESSELS")

  isdbTabs = c("ISFISHSETS",
               "ISTRIPS",
               "ISVESSELS",
               "ISCATCHES",
               "ISGEARS")

  obsTabs = c("ISSETPROFILE_WIDE")

  args[["marfTabs"]] <- marfTabs
  args[["isdbTabs"]] <- isdbTabs
  args[["obsTabs"]] <- obsTabs
  ISDB = paste0("ISDB.",isdbTabs)
  MARFIS = paste0("MARFISSCI.",marfTabs)
  OBS = paste0("OBSERVER.",obsTabs)

  if (args$useLocal){
    if (do.call(wantLocal,list(MARFIS,args=args)) & do.call(wantLocal,list(ISDB,args=args)) & do.call(wantLocal,list(OBS,args=args))){
      args[['cxn']] <- TRUE
      res <- args
      if (args$debug){
        t28_ <- proc.time() - t28
        message("\tExiting can_run() (",round(t28_[1],0),"s elapsed)")
      }
      return(res)
    }else{
      message(paste0("Cannot proceed offline. Check that all of the following files are in your data.dir (",args$data.dir,"):\n"))
      message(paste0(paste0(MARFIS,".RData"),collapse = "\n"))
      message(paste0(paste0(ISDB,".RData"),collapse = "\n"))
      message(paste0(paste0(OBS,".RData"),collapse = "\n"))
      stop()
    }
  }else{
    cxnCheck <- do.call(connect_Oracle, list(args=args))
    if (!is.list(cxnCheck)) {
      message("\n","Can't create a DB connection - see suggestions below: ",
              "\n\t","1) Please ensure you've provided valid values for oracle.username and/or oracle.password.",
              "\n\t","2) Note that if not provided, 'oracle.dsn' defaults to 'PTRAN' which might be different on your computer.",
              "\n\t","3) Note that 'usepkg' defaults to the value 'rodbc', indicating the RODBC package should be used to created a connection.",
              "\n\t","   If you use ROracle, include 'usepkg='roracle' as a parameter to your function.",
              "\n\t","4) If you see an error message below that includes the text 'architecture mismatch', you likely need to switch your ",
              "\n\t","   R from 32bit to 64bit (or vice versa), and try again."
      )
      stop()
    }else{
      args[['cxn']] <- cxnCheck
      res <- args

      if (args$debug){
        t28_ <- proc.time() - t28
        message("\tExiting can_run() - db cxn established (", round(t28_[1],0),"s elapsed)")
      }
      return(res)
    }
    if (all(do.call(tblAccess,list(MARFIS, args=args)) && do.call(tblAccess,list(ISDB, args=args)) && do.call(tblAccess,list(OBS, args=args)))){
      message("\n","Connected to DB, and verified that account has sufficient permissions to proceed.","\n")
      res <- args
      if (args$debug){
        t28_ <- proc.time() - t28
        message("\tExiting can_run() - tbl access verified (", round(t28_[1],0),"s elapsed)")
      }

           return(res)
    }else{
      message("\n","Connected to DB, but account does not have sufficient permissions to proceed.","\n")
      stop()
    }
  }
}


#' @title enable_local
#' @description This function extracts all of the necessary oracle tables to a local folder so that
#' the functions can be run locally.
#' @param data.dir  The default is your working directory. If you are hoping to
#' load existing data, this folder should identify the folder containing your
#' *.rdata files.
#' @param oracle.username default is \code{'_none_'} This is your username for
#' accessing oracle objects. If you have a value for \code{oracle.username}
#' stored in your environment (e.g. from an rprofile file), this can be left out
#' and that value will be used.  If a value for this is provided, it will take
#' priority over your existing value.
#' @param oracle.password default is \code{'_none_'} This is your password for
#' accessing oracle objects. If you have a value for \code{oracle.password}
#' stored in your environment (e.g. from an rprofile file), this can be left out
#' and that value will be used.  If a value for this is provided, it will take
#' priority over your existing value.
#' @param oracle.dsn default is \code{'_none_'} This is your dsn/ODBC
#' identifier for accessing oracle objects. If you have a value for
#' \code{oracle.dsn} stored in your environment (e.g. from an rprofile file),
#' this can be left and that value will be used.  If a value for this is
#' provided, it will take priority over your existing value.
#' @param usepkg default is \code{'rodbc'}. This indicates whether the connection to Oracle should
#' use \code{'rodbc'} or \code{'roracle'} to connect.  rodbc is slightly easier to setup, but
#' roracle will extract data ~ 5x faster.
#' @param ... other arguments passed to methods
#' @family setup
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
enable_local <- function(data.dir = NULL,
                         oracle.username = "_none_",
                         oracle.password = "_none_",
                         oracle.dsn = "_none_",
                         usepkg = "rodbc",
                         ...){
  # browser("Check if args exists(probly no), and ensure it gets merged with args below")
  # args <-list(...)$args
  # if (args$debug) t32 <- Mar.utils::where_now(returnTime = T)
  if (is.null(data.dir)|
      oracle.username == "_none_"|
      oracle.password == "_none_" |
      oracle.dsn == "_none_")stop("Can't run as requested.")

  args <- list(data.dir = data.dir,
               oracle.username = oracle.username,
               oracle.password = oracle.password,
               oracle.dsn = oracle.dsn,
               usepkg = usepkg,
               debug = FALSE,
               quietly = TRUE,
               useLocal = FALSE
  )

  can_runCheck <- do.call(can_run, args)
  args <- can_runCheck
  cxnCheck <- args$cxn
  if (!(is.list(cxnCheck) || cxnCheck==TRUE)){
    stop("Can't run as requested.")
  }
  message("\nChecking for and/or extracting MARFIS data...\n")
  Mar.utils::get_data_tables(fn.oracle.username = oracle.username,
                             fn.oracle.password = oracle.password,
                             fn.oracle.dsn = oracle.dsn,
                             usepkg = usepkg,
                             schema = "MARFISSCI",
                             data.dir = data.dir,
                             checkOnly = FALSE,
                             tables = args$marfTabs,
                             env = environment(), quietly = FALSE, fuzzyMatch=FALSE)

  message("\nChecking for and/or extracting ISDB data...\n")
  Mar.utils::get_data_tables(fn.oracle.username = oracle.username,
                             fn.oracle.password = oracle.password,
                             fn.oracle.dsn = oracle.dsn,
                             usepkg = usepkg,
                             schema = "ISDB",
                             data.dir = data.dir,
                             checkOnly = FALSE,
                             tables = args$isdbTabs,
                             env = environment(), quietly = FALSE, fuzzyMatch=FALSE)

  message("\nChecking for and/or extracting Observer data...\n")
  Mar.utils::get_data_tables(fn.oracle.username = oracle.username,
                             fn.oracle.password = oracle.password,
                             fn.oracle.dsn = oracle.dsn,
                             usepkg = usepkg,
                             schema = "OBSERVER",
                             data.dir = data.dir,
                             checkOnly = FALSE,
                             tables = args$obsTabs,
                             env = environment(), quietly = FALSE, fuzzyMatch=FALSE)
  message(paste0("\nConfirmed presence of all necessary tables in ", data.dir),"\n")
#   if (args$debug) {
#   t32_ <- proc.time() - t32
#   message("\tExiting enable_local() (",round(t32_[1],0),"s elapsed)")
# }
}

#' @title summarize_locations
#' @description This function takes the results from get_marfis() and get_isdb()
#' and produces a table showing the how the proportion of observed data varies
#' across areas.  Results for NAFO areas are always provided, as these areas are
#' stored in both MARFIS and ISDB databases.  If a custom area is specified (via the
#' \code{areaFile} parameter), the sets from both the MARFIS and ISDB data are analyzed
#' for that area too (as a separate dataframe).
#' @param get_marfis default is \code{NULL}. This is the list output by the
#' \code{Mar.fleets::get_marfis()} function - it contains dataframes of both the
#' trip and set information from MARFIS related to the specified fleet
#' @param get_isdb default is \code{NULL}. This is the list output by the
#' \code{Mar.fleets::get_isdb()} function - it contains dataframes of both the
#' trip and set information from ISDB related to the specified fleet

#' @param ... other arguments passed to methods
#' @family coreFuncs
#' @return returns a list with 1 or 2 dataframes. These dataframes indicate how much of
#' the fishing activity took place in various areas.  Values are categorized as both
#' "*_reported" and "*_calculated".  "Reported" values are based on the NAFO information
#' stored in MARFIS and ISDB.  These are available for MARFIS trips, MARFIS sets, and ISDB sets.
#' "Calculated" values are based on the coordinates associated with the data, and are only
#' available for the MARFIS and ISDB sets (not trips).
#' \itemize{
#' \item \code{NAFO} =  This is always provided.
#' \item \code{<Mar.data::object_name>} = This is only provided when a specific "areaFile" is
#' provided as a paramater.
#' }
#' @note
#' \itemize{
#' \item \code{_SETS} Coordinates for MARFIS_* data comes from the db table "LOG_EFRT_STD_INFO"
#' (preferring *_ENT coordinates over *_DET ), and coordinates for ISDB_* data come from
#' "ISSETPROFILE" - using the first non-null pair found while checking from P1-P4 in order.
#' \item \code{Weird Coordinates} In addition to falling within a known area, data can also be
#' categorized any of the following:
#' \itemize{
#' \item \code{<missing coord>}  These are missing values for with LAT or LONG (or both)
#' \item \code{<impossible coord>}  These have latitudes or longitudes outside of the acceptable
#' ranges of 90 - -90  and 180 - -180, respectively
#' \item \code{<on boundary line>}  These coordinates fall exactly on a boundary line dividing 2
#' areas
#' \item \code{<LAND>} For NAFO areas, points that fall on land (according to
#' Mar.data::NAFOSubunitsLnd_sf) are indicated as such
#' \item \code{<outside known areas>} These are points that fall outside any of the known polygons
#' for the specified area. For non_NAFO areas, this can include land.
#' }
#' }
#' @family simpleproducts
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
summarize_locations<-function(get_isdb = NULL,
                              get_marfis = NULL,
                              ...){
  args <- list(...)$args
  if (args$debug) t33 <- Mar.utils::where_now(returnTime = T)
  trimNAFONames<- function(nafoString = NULL, det = det){
    if (args$debug) tx<- Mar.utils::where_now(returnTime = T)
    toTrim <- nafoString[nchar(nafoString)<=4]
    noTrim <- nafoString[!(nafoString %in% toTrim)]
    toTrim <- substr(toTrim, 1,det)
    res <- sort(c(toTrim, noTrim))
    if (args$debug) {
      t33_ <- proc.time() - t33
    message("\tExiting trimNAFONames() (",round(t33_[1],0),"s elapsed)")
    }
    return(res)
  }

  mSets <- get_marfis$MARF_SETS[,c("LOG_EFRT_STD_INFO_ID","NAFO_MARF_SETS","NAFO_MARF_SETS_CALC")]
  mSets$NAFO_MARF_SETS <- trimNAFONames(mSets$NAFO_MARF_SETS, args$nafoDet)
  mSets$NAFO_MARF_SETS_CALC <- trimNAFONames(mSets$NAFO_MARF_SETS_CALC, args$nafoDet)

  mSetsSummRpt <- stats::aggregate(
    x = list(cnt = mSets$LOG_EFRT_STD_INFO_ID ),
    by = list(NAFO = mSets$NAFO_MARF_SETS),
    length
  )
  mSetsSummRpt$SRC <- "mSetsSummRpt"

  mSetsSummCalc <- stats::aggregate(
    x = list(cnt = mSets$LOG_EFRT_STD_INFO_ID ),
    by = list(NAFO = mSets$NAFO_MARF_SETS_CALC),
    length
  )
  mSetsSummCalc$SRC <- "mSetsSummCalc"

  if (any(!is.na(get_isdb$ISDB_SETS))){
    oSets <- get_isdb$ISDB_SETS[,c("FISHSET_ID","NAFO_ISDB_SETS", "NAFO_ISDB_SETS_CALC")]
    oSets[!is.na(oSets$NAFO_ISDB_SETS),"NAFO_ISDB_SETS"] <- trimNAFONames(oSets[!is.na(oSets$NAFO_ISDB_SETS),"NAFO_ISDB_SETS"], args$nafoDet)
    oSets[!is.na(oSets$NAFO_ISDB_SETS_CALC),"NAFO_ISDB_SETS_CALC"] <- trimNAFONames(oSets[!is.na(oSets$NAFO_ISDB_SETS_CALC),"NAFO_ISDB_SETS_CALC"] , args$nafoDet)
  }else{
    #making a zero row df
    oSets <-data.frame("nothing" = logical())
  }
  if (nrow(oSets)>0){
    oSetsSummRpt <- stats::aggregate(
      x = list(cnt = oSets$FISHSET_ID ),
      by = list(NAFO = oSets$NAFO_ISDB_SETS
      ),
      length
    )
    oSetsSummRpt$SRC <- "oSetsSummRpt"

    oSetsSummCalc <- stats::aggregate(
      x = list(cnt = oSets$FISHSET_ID ),
      by = list(NAFO = oSets$NAFO_ISDB_SETS_CALC
      ),
      length
    )
    oSetsSummCalc$SRC <- "oSetsSummCalc"
  }else{
    oSetsSummRpt <- oSetsSummCalc <- data.frame("cnt" = integer(), "NAFO" = character(), "SRC" = character())
  }

  # all <- rbind.data.frame(mTripsSummRpt, )
  all <- rbind.data.frame(mSetsSummRpt, mSetsSummCalc)
  all <- rbind.data.frame(all, oSetsSummRpt)
  all <- rbind.data.frame(all, oSetsSummCalc)


  summary <- reshape2::dcast(all, NAFO ~ SRC, value.var = "cnt")
  summary[is.na(summary)] <- 0
  if (!"oSetsSummRpt" %in% colnames(summary)) summary$oSetsSummRpt <- 0
  if (!"oSetsSummCalc" %in% colnames(summary)) summary$oSetsSummCalc <- 0
  summary = summary[with(summary, order(NAFO)), c("NAFO", "mSetsSummRpt" , "mSetsSummCalc" , "oSetsSummRpt", "oSetsSummCalc")]

  rowsNAFO <- summary[!grepl("^<", summary$NAFO),]
  rowsOther <- summary[!(summary$NAFO %in% rowsNAFO$NAFO),]

  if (nrow(rowsNAFO)>0 & nrow(rowsOther)>0){
    summary <- rbind.data.frame(rowsNAFO, rowsOther)
  }

  # colnames(summary)[colnames(summary)=="mTripsSummRpt"] <- "TRIPS_MARF_reported"
  colnames(summary)[colnames(summary)=="mSetsSummRpt"] <- "SETS_MARF_reported"
  colnames(summary)[colnames(summary)=="mSetsSummCalc"] <- "SETS_MARF_calculated"
  colnames(summary)[colnames(summary)=="oSetsSummRpt"] <- "SETS_ISDB_reported"
  colnames(summary)[colnames(summary)=="oSetsSummCalc"] <- "SETS_ISDB_calculated"
  res <- list()
  res[["NAFO"]] <- summary
  if (args$areaFile != "NAFOSubunits_sf" | args$areaFileField != "NAFO_1"){
    mSets_cust <- get_marfis$MARF_SETS[,c("LOG_EFRT_STD_INFO_ID",args$areaFileField)]
    mSetsSummRpt_cust <- stats::aggregate(
      x = list(cnt = mSets_cust$LOG_EFRT_STD_INFO_ID ),
      by = list(tmpName = mSets_cust[,args$areaFileField]),
      length
    )
    mSetsSummRpt_cust$SRC <- "MARFIS_sets"

    oSets_cust <- get_isdb$ISDB_SETS[,c("FISHSET_ID",args$areaFileField)]


    oSetsSummRpt_cust <- stats::aggregate(
      x = list(cnt = oSets_cust$FISHSET_ID ),
      by = list(tmpName = oSets_cust[,args$areaFileField]),
      length
    )
    oSetsSummRpt_cust$SRC <- "ISDB_sets"

    all_cust <- rbind.data.frame(mSetsSummRpt_cust, oSetsSummRpt_cust)
    summary_cust <- reshape2::dcast(all_cust, tmpName ~ SRC, value.var = "cnt")
    summary_cust[is.na(summary_cust)] <- 0
    summary_cust <- summary_cust[with(summary_cust, order(tmpName)), c("tmpName", "MARFIS_sets" , "ISDB_sets")]

    colnames(summary_cust)[colnames(summary_cust)=="tmpName"] <- args$areaFileField
    if(grepl(pattern = ".shp",x = args$areaFile, ignore.case = T)){
      custName <- "customShpFile"
    }else{
      custName <- args$areaFile
    }
    res[[custName]] <- summary_cust
  }


  if (args$debug) {
    t33_ <- proc.time() - t33
    message("\tExiting summarize_locations() (",round(t33_[1],0),"s elapsed)")
  }
  return(res)
}
