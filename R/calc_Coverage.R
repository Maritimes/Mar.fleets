#' @title calc_Coverage
#' @description This function takes the results from get_MARFIS() and get_OBS()
#' and produces a table showing the how the proportion of observed data varies
#' across areas.  Different polygons can be provided, and the results will be
#' tailored to those polygons.
#' @param get_MARFIS default is \code{NULL}. This is the list output by the
#' \code{Mar.bycatch::get_MARFIS()} function - it contains dataframes of both the
#' trip and set information from MARFIS related to the specified fleet
#' @param get_OBS default is \code{NULL}. This is the list output by the
#' \code{Mar.bycatch::get_OBS()} function - it contains dataframes of both the
#' trip and set information from OBSERVER related to the specified fleet
#' @param agg.poly.shp default is \code{NULL}.  This is the shapefile that has
#' polygons that should be checked for sufficient unique values of the
#' sens.fields.  If NULL, NAFO zones will be used.  Otherwise, a path to any
#' polygon shapefile can be provided.
#' @param agg.poly.field default is \code{NULL}.  This identifies the field within
#' the shapefile provided to agg.poly.shp that should be used to check for
#' sufficient unique values of the sens.fields.
#' @param quietly default is \code{FALSE}.  This indicates whether or not
#' information about the matching process should be shown.
#' @family fleets
#' @return returns a list with 2 items - \code{summary} and \code{details}.
#'
#' \code{summary} is a dataframe with 5 columns, as described below:
#' \itemize{
#' \item {..name dependent on input..} =  named the same as \code{agg.poly.field} and
#' contains all of the values of \code{agg.poly.field}
#' \item \code{MARFIS_TRIPS} {This is how many Commercial trips had their majority of
#' their sets within this polygon}
#' \item \code{OBS_TRIPS} {This is how many opbserved trips had their majority of
#' their sets within this polygon}
#' \item \code{MARFIS_SETS} {This is how many Commercial sets occurred in this polygon}
#' \item \code{OBS_TRIPS} {This is how many Observed sets occurred in this polygon}
#' }
#'
#' \code{details} is a list containing 4 dataframes - MARFIS_TRIPS, OBS_TRIPS,
#' MARFIS_SETS and OBS_SETS.  Each of these dataframes has 2 columns - the first
#' identifying the TRIP or SET, and the second identifying which area was
#' applied to that TRIP OR SET.
#'
#' @note
#' \itemize{
#' \item \code{_SETS} A given "TRIP_ID" in MARFIS or OBSERVER
#' typically has multiple sets, and each set has its own coordinates.  For this
#' analysis, every set is analyzed to see which polygon it occurred in, and
#' this information is provided via as the values for \code{MARFIS_SETS} and
#' \code{OBS_SETS} in the results.
#'
#' Coordinates for MARFIS_* data comes from the db table "PRO_SPC_INFO", and
#' coordinates for OBS_* data come from "ISSETPROFILE" - using the first non-null
#' pair found while checking from P1-P4 in order.
#'
#' \item \code{_TRIPS} = Since a single trip can have sets across
#' multiple polygons, this script determines which polygon had the most sets for
#' a given trip, and defines that polygon as the value for a given trip.  These
#' are provided as the values for \code{MARFIS_TRIPS} and \code{OBS_TRIPS}.
#'
#' \item Bad\code{Weird} Coordinates = If the latitude or longitude of the input data
#' for any set has a NULL value, an entry of "Bad coordinate" will be added to
#' the results.
#' Additionally, if fishing can occur outside of the extent of the provided polygon,
#' those trips/sets will be attributed to "Other".
#' }
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
calc_Coverage<-function(get_OBS = NULL,
                          get_MARFIS = NULL,
                          agg.poly.shp = NULL,
                          agg.poly.field = NULL,
                          use.matched =T,
                          quietly = FALSE){
  if (use.matched){
    oTrips <- get_OBS$OBS_TRIPS_MATCHED
    oSets <- get_OBS$OBS_SETS_MATCHED
  }else{
    oTrips <- get_OBS$OBS_TRIPS
    oSets <- get_OBS$OBS_SETS
  }
  .I <- LOG_EFRT_STD_INFO_ID <- MON_DOC_ID<- cnt<- TRIP_ID <-NA
    if (!is.null(agg.poly.shp)){
      agg.poly <- rgdal::readOGR(dsn = agg.poly.shp, verbose = FALSE)
    }else{
      agg.poly <- Mar.data::NAFOSubunits
    }
   if (is.null(agg.poly.field))agg.poly.field="NAFO_1"

    allAreas = as.data.frame(sort(agg.poly@data[,agg.poly.field]))
    colnames(allAreas)[1] <- agg.poly.field
    allAreas = rbind(allAreas, "Other")
    allAreas = rbind(allAreas, "Bad coordinate")
    #by set

    if (!quietly)cat(paste0("\n", "Figuring out which area each set occurred in..."))
    if (!is.null(oSets)){
      OBS_area_s = Mar.utils::identify_area(oSets,
                                     agg.poly.shp = agg.poly.shp,
                                     agg.poly.field = agg.poly.field)
    }else{
      OBS_area_s<- NA
    }
  #grab the first (valid) position from each set
  if (!is.null(get_MARFIS$MARF_SETS)){
    MARF_sets_pos <- get_MARFIS$MARF_SETS[,c("MON_DOC_ID","TRIP_ID_MARF","LOG_EFRT_STD_INFO_ID","LATITUDE","LONGITUDE")]
    MARF_sets_pos <- MARF_sets_pos[with(MARF_sets_pos,order(LOG_EFRT_STD_INFO_ID)),]
    MARF_sets_pos <- MARF_sets_pos[!is.na(MARF_sets_pos$LATITUDE) & !is.na(MARF_sets_pos$LONGITUDE),]
    MARF_sets_pos <-data.table::as.data.table(MARF_sets_pos)
    MARF_sets_pos <- MARF_sets_pos[MARF_sets_pos[, .I[LOG_EFRT_STD_INFO_ID == min(LOG_EFRT_STD_INFO_ID)], by=MON_DOC_ID]$V1]
    MARF_sets_pos <- as.data.frame(MARF_sets_pos)
    MARF_sets_pos$LOG_EFRT_STD_INFO_ID <- NULL
    MARF_sets_pos <- unique(MARF_sets_pos[!is.na(MARF_sets_pos$MON_DOC_ID),])
    Marf_Trip_pos<-merge(get_MARFIS$MARF_TRIPS, MARF_sets_pos )
    MAR_area_s = Mar.utils::identify_area(get_MARFIS$MARF_SETS,
                                        agg.poly.shp = agg.poly.shp,
                                        agg.poly.field = agg.poly.field)
  }else{
    Marf_Trip_pos<-merge(get_MARFIS$MARF_TRIPS, MARF_sets_pos )
    MAR_area_s<- NA
  }


  #by_trip
  if (!quietly)cat(paste0("\n", "Figuring out the area in which the most sets occurred during each trip.","\n"))

  if (!is.null(oTrips) && !is.null(oSets)){
    O_trips = merge(oTrips[,!names(oTrips) %in% c("BOARD_DATE","LANDING_DATE")],
                    oSets, all.y =TRUE, by.x = "TRIP_ID_OBS", by.y = "TRIP_ID")
    OBS_area_t = Mar.utils::identify_area(O_trips,
                                        agg.poly.shp = agg.poly.shp,
                                        agg.poly.field = agg.poly.field)
  }else{
    OBS_area_t<-NA
  }

  MAR_area_t = Mar.utils::identify_area(Marf_Trip_pos,
                                      agg.poly.shp = agg.poly.shp,
                                      agg.poly.field = agg.poly.field)

  determineArea<-function(df=NULL, setField = NULL,  agg.poly.field = NULL, newID = NULL){
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
  coverageValues = stats::aggregate(
    x = list(tmp = df_new$TRIP_ID),
    by = list(area = df_new$area),
    length
  )
  df_new[df_new$area == 99999,"area"]<-"Other"
  colnames(df_new)[colnames(df_new)=="area"] <- agg.poly.field
  colnames(df_new)[colnames(df_new)=="TRIP_ID"] <- setField
  coverageValues[coverageValues$area == 99999,"area"]<-"Other"
  colnames(coverageValues)[colnames(coverageValues)=="area"] <- agg.poly.field
  colnames(coverageValues)[colnames(coverageValues)=="tmp"] <- newID
  colnames(coverageValues)[colnames(coverageValues)=="TRIP_ID"] <- setField
  res = list()
  res[["summary"]]<-coverageValues
  res[["details"]]<-df_new
  return(res)
  }
  if(is.data.frame(MAR_area_t)){
    m_t = determineArea(df = MAR_area_t, setField = "TRIP_ID_MARF" , agg.poly.field = agg.poly.field, newID = "MARFIS_TRIPS")
    allAreas = merge(allAreas,m_t$summary, by= agg.poly.field, all.x=T)
  }else{
    m_t<-NA
  }
  if(is.data.frame(OBS_area_t)){
    o_t = determineArea(df = OBS_area_t, setField = "TRIP_ID_OBS" , agg.poly.field = agg.poly.field, newID = "OBS_TRIPS")
    allAreas = merge(allAreas,o_t$summary, by= agg.poly.field, all.x=T)
  }else{
    o_t<-NA
  }
  if(is.data.frame(MAR_area_s)){
    m_s = determineArea(df = MAR_area_s, setField = "LOG_EFRT_STD_INFO_ID" , agg.poly.field = agg.poly.field, newID = "MARFIS_SETS")
    allAreas = merge(allAreas,m_s$summary, by= agg.poly.field, all.x=T)
  }else{
    m_s<-NA
  }
  if(is.data.frame(OBS_area_s)){
     o_s = determineArea(df = OBS_area_s, setField = "FISHSET_ID" , agg.poly.field = agg.poly.field, newID = "OBS_SETS")
     allAreas = merge(allAreas,o_s$summary, by= agg.poly.field, all.x=T)
  }else{
    o_s<-NA
  }


  #remove rows with no useful values
  allAreas =  unique(allAreas[rowSums(is.na(allAreas[,2:ncol(allAreas)]))!=ncol(allAreas)-1,])
  res = list()
  res[["summary"]]<-allAreas
  res[["details"]]<-list()
  res$details[["TRIPS_MARF"]] <- ifelse(!is.na(m_t),m_t, NA)
  res$details[["TRIPS_OBS"]] <- ifelse(!is.na(o_t),o_t, NA)
  res$details[["SETS_MARF"]] <- ifelse(!is.na(m_s),m_s, NA)
  res$details[["SETS_OBS"]] <- ifelse(!is.na(o_s),o_s, NA)
  return(res)
}
