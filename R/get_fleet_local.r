#' @title get_fleet_local
#' @description This function extracts all of the Vessel/Licence combinations
#' associated with a particular fleet for a particular date range.
#' @param ... other arguments passed to methods
#' @family fleets
#' @return returns a data.frame with 6 columns - "GEAR_CODE", "GEAR_DESC",
#'         "MD_CODE", "MD_DESC", "VR_NUMBER", "LICENCE_ID"
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_fleet_local<-function(...){
  args<-  list(...)$argsList
  df <- do.call(get_fleetBasic_local, list(argsList=args))
  bad = c("MONIT.*","DOCU.*","/ .*","FISHING .*","LOG.*"," FI$")
  for (b in 1:length(bad)){
    df$MD_DESC = sub(bad[b], "", df$MD_DESC)
  }
  df$MD_DESC <- trimws(df$MD_DESC)
  df <- do.call(applyFilters, list(df=df,argsList=args))

  if(nrow(df)<1) {
    cat(paste0("\n","No records found"))
    return(NULL)
  }else{
    df$NAFO <-NULL
    df <- unique(df[with(df,order(VR_NUMBER, LICENCE_ID, MD_CODE, GEAR_CODE )),])
    # cat("Final: ",nrow(df),"\n")
    return(df)
  }
}
