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
