#' @ title plot_coverage_time
#' @description This function takes the results from get_marfis() and get_isdb()
#' and extracts the relevant VMS tracks, and flags whether or not each was observed
#' @param get_marfis default is \code{NULL}. This is the list output by the
#' \code{Mar.fleets::get_marfis()} function - it contains dataframes of both the
#' trip and set information from MARFIS
#' @param get_isdb default is \code{NULL}. This is the list output by the
#' \code{Mar.fleets::get_isdb()} function - it contains dataframes of both the
#' trip and set information from the ISDB database.
#' @param dateStart default is \code{NULL}. This is the start of the date range for which marfis and
#' ISDB was searched for (e.g. "YYYY-MM-DD"). If missing, it will use the earliest date from the
#' supplied data.
#' @param dateEnd default is \code{NULL}. This is the end of the date range for which marfis and
#' ISDB was searched for (e.g. "YYYY-MM-DD"). If missing, it will use the most recent date from the
#' supplied data.
#' @param useDate default is \code{"LANDED_DATE"}.  Some MARFIS tables have 2 different dates
#' that are used for recording when fishing activity took place.  One is "DATE_FISHED",
#' and the other is "LANDED_DATE".Whichever is chosen will be the field used to aggregate the MARFIS
#' trips.  This can be important for particular fleets, and it would be best if this value matched
#' whatever was used for the initial extraction of the \code{get_isdb} and \code{get_marfis} objects.
#' @family simpleproducts
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
plot_coverage_time<- function(get_marfis = NULL, get_isdb=NULL,  dateStart=NULL, dateEnd = NULL, useDate = "LANDED_DATE"){

  MARF_T <- MARF_S <- OBS_T <- OBS_S <- DATE <- NA
  if (is.null(dateStart)|is.null(dateEnd)){

    dateRange <-range(c(as.Date(range(get_marfis$MARF_TRIPS[,useDate])),
                        as.Date(range(get_marfis$MARF_SETS[,"EF_FISHED_DATETIME"])),
                        as.Date(range(get_isdb$ALL_ISDB_TRIPS[,"BOARD_DATE"])),
                        as.Date(range(get_isdb$ALL_ISDB_SETS[,"DATE_TIME"]))),na.rm = T)
    if (is.null(dateStart)) dateStart <- min(dateRange)
    if (is.null(dateEnd)) dateEnd <- max(dateRange)
    cat("\n","Date ranges defaulting to match the extents of provided data")
  }

  dateAgger <- function(df=NULL, theN= NULL, theDate=NULL, dateStart = NULL, dateEnd = NULL){
    # this guy takes a dataframe and calculates how many events happened on each day
    # missing dates are populated with zeroes
    dfColName <- switch(theN,
                        "PRO_SPC_INFO_ID" = "MARF_T",
                        "LOG_EFRT_STD_INFO_ID" = "MARF_T",
                        "TRIP_ID_ISDB" = "OBS_T",
                        "FISHSET_ID" = "OBS_S")

    this = stats::aggregate(
      x = list(N = df[,theN]),
      by = list(DATE = as.Date(df[,theDate])),
      length
    )

    allDates<- data.frame(DATE=seq(as.Date(dateStart), as.Date(dateEnd), by="days"))
    this <- merge(this,allDates,by.x='DATE',by.y='DATE',all.x=T,all.y=T)
    this$N[is.na(this$N)] <- 0
    this = this[with(this, order(DATE)), ]
    colnames(this)[colnames(this)=="N"] <- dfColName
    return(this)
  }

  MT = dateAgger(df=get_marfis$MARF_TRIPS, theN="PRO_SPC_INFO_ID", theDate = useDate, dateStart = dateStart, dateEnd = dateEnd)
  MS = dateAgger(df=get_marfis$MARF_SETS, theN="LOG_EFRT_STD_INFO_ID", theDate = "EF_FISHED_DATETIME", dateStart = dateStart, dateEnd = dateEnd)
  OT = dateAgger(df=get_isdb$ALL_ISDB_TRIPS, theN="TRIP_ID_ISDB", theDate = "BOARD_DATE", dateStart = dateStart, dateEnd = dateEnd)
  OS = dateAgger(df=get_isdb$ALL_ISDB_SETS, theN="FISHSET_ID", theDate = "DATE_TIME", dateStart = dateStart, dateEnd = dateEnd)

  all= merge(MT, MS)
  all= merge(all, OT)
  all= merge(all, OS)

  # Plots ---------------------------------------------------------------------------------------
  trips <- ggplot2::ggplot(data = all, mapping = ggplot2::aes(x=DATE))
  trips <- trips + ggplot2::labs(y="Number of Trips", x = "DATE", title = "Observed vs Unobserved Trips")
  trips <- trips + ggplot2::geom_col(ggplot2::aes(y = MARF_T, fill = "UNOBSERVED"))
  trips <- trips + ggplot2::geom_col(ggplot2::aes(y = OBS_T, fill = "OBSERVED"))
  trips <- trips + ggplot2::scale_fill_manual(name = "", values = c("UNOBSERVED" = "blue", "OBSERVED" = "red"))
  trips <- trips + ggplot2::scale_x_date(labels = scales::date_format("%b %Y"),breaks = "1 month")
  trips <- trips + ggplot2::theme_minimal()
  trips <- trips + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust=0.25))

  ggplot2::ggsave(plot = trips, filename = "trips.png", width = 14, height=8.5)

  sets <- ggplot2::ggplot(all,  mapping = ggplot2::aes(DATE))
  sets <- sets + ggplot2::labs(y="Number of Sets", x = "DATE", title = "Observed vs Unobserved Sets")
  sets <- sets + ggplot2::geom_col(ggplot2::aes(y = MARF_S, fill = "UNOBSERVED"))
  sets <- sets + ggplot2::geom_col(ggplot2::aes(y = OBS_S, fill = "OBSERVED"))
  sets <- sets + ggplot2::scale_fill_manual(name = "", values = c("UNOBSERVED" = "blue", "OBSERVED" = "red"))
  sets <- sets + ggplot2::scale_x_date(labels = scales::date_format("%b %Y"),breaks = "1 month")
  sets <- sets + ggplot2::theme_minimal()
  sets <- sets + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust=0.25))

  ggplot2::ggsave(plot = sets, filename = "sets.png", width = 14, height=8.5)
  cat("\n\n", "Because the Observer data can be matched on one of many different fields (i.e. Trip Name,
Hail In or Out IDs, or a combination of VRN, Licence and Date range), it is possible for these plots
to show observed trips on days when there appear to be no MARFIS trips. Matches by VRN, licence
and date range only require that the MARFIS trips fall within the range of an Observed trip - the
plotted day for each trip may differ." )
  return(all)

}
