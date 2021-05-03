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

#' @title go
#' @description This function prevents people from running the package until it's ready
#' @family utils
#' @return returns TRUE or FALSe, and informative messages
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
go <- function(){
  res <- FALSE
  inf <- as.list(Sys.info())
  if (inf$login %in% c("mcmahonm","bowlbyh"))res <- TRUE
  return(res)
}
