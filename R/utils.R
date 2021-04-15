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
#' @export
clean_ISDB_Trip <- function(df=NULL, field = "ISDB_TRIP", out_name="ISDB_TRIP_CLN"){
  df[,out_name] <- gsub(pattern = "[^[:alnum:]]", replacement = "", x=  df[,field])
  return(df)
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


determineArea<-function(df=NULL, setField = NULL,  agg.poly.field = NULL, newID = NULL){
  browser()
  clean_df_field <- function(df=df, agg.poly.field = NULL){
    #this populates NA fields with a known (bad) value so we can track how many there were
    df[[agg.poly.field]][is.na(df[[agg.poly.field]])] <- 99999
    return(df)
  }
  df = clean_df_field(df, agg.poly.field)
  df_new = stats::aggregate(
    x = list(cnt =  df[,setField]),
    by = list(TRIP_ID = df[,setField],
              area =  df[,agg.poly.field]
    ),
    length
  )
  #The following assigns each trip to the area
  #with the most sets.

  df_new <- data.table::setDT(df_new)
  df_new <- df_new[df_new[, .I[which.max(cnt)], by=TRIP_ID]$V1]
  df_new <- as.data.frame(df_new)
  df_new$cnt<-NULL
  locValues = stats::aggregate(
    x = list(tmp = df_new$TRIP_ID),
    by = list(area = df_new$area),
    length
  )
  df_new[df_new$area == 99999,"area"]<-"Other"
  colnames(df_new)[colnames(df_new)=="area"] <- agg.poly.field
  colnames(df_new)[colnames(df_new)=="TRIP_ID"] <- setField
  locValues[locValues$area == 99999,"area"]<-"Other"
  colnames(locValues)[colnames(locValues)=="area"] <- agg.poly.field
  colnames(locValues)[colnames(locValues)=="tmp"] <- newID
  colnames(locValues)[colnames(locValues)=="TRIP_ID"] <- setField
  res = list()
  res[["summary"]]<-locValues
  res[["details"]]<-df_new
  return(res)
}
