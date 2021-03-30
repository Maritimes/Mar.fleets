#' @title summarize_locations
#' @description This function takes the results from get_marfis() and get_isdb()
#' and produces a table showing the how the proportion of observed data varies
#' across areas.  Different polygons can be provided, and the results will be
#' tailored to those polygons.
#' @param get_marfis default is \code{NULL}. This is the list output by the
#' \code{Mar.bycatch::get_marfis()} function - it contains dataframes of both the
#' trip and set information from MARFIS related to the specified fleet
#' @param get_isdb default is \code{NULL}. This is the list output by the
#' \code{Mar.bycatch::get_isdb()} function - it contains dataframes of both the
#' trip and set information from ISDB related to the specified fleet
#' @param marfMatchOnly default is \code{TRUE}.  This determines whether the
#' function should be run on all ISDB data, or just on those that have been
#' matched to MARFIS data (the default).
#' @param agg.poly.shp default is \code{NULL}.  This is either the path to the
#' *.shp file of a shapefile, an sf spatial object; or an sp spatialpolygonsdataframe.
#'  If NULL, NAFO zones will be used.
#' @param agg.poly.field default is \code{NULL}.  This identifies the field within
#' \code{agg.poly.shp} that contains the values that should be appended to the
#' input dataframe. If NULL, "NAFO_BEST", will be used, which is the finest
#' resolution NAFO subdivision.
#' @param ... other arguments passed to methods
#' @family simpleproducts
#' @return returns a list with 2 items - \code{summary} and \code{details}.
#'
#' \code{summary} is a dataframe with 5 columns, as described below:
#' \itemize{
#' \item {..name dependent on input..} =  named the same as \code{agg.poly.field} and
#' contains all of the values of \code{agg.poly.field}
#' \item \code{MARFIS_TRIPS} {This is how many Commercial trips had their majority of
#' their sets within this polygon}
#' \item \code{ISDB_TRIPS} {This is how many opbserved trips had their majority of
#' their sets within this polygon}
#' \item \code{MARFIS_SETS} {This is how many Commercial sets occurred in this polygon}
#' \item \code{ISDB_TRIPS} {This is how many Observed sets occurred in this polygon}
#' }
#'
#' \code{details} is a list containing 4 dataframes - MARFIS_TRIPS, ISDB_TRIPS,
#' MARFIS_SETS and ISDB_SETS.  Each of these dataframes has 2 columns - the first
#' identifying the TRIP or SET, and the second identifying which area was
#' applied to that TRIP OR SET.
#'
#' @note
#' \itemize{
#' \item \code{_SETS} A given "TRIP_ID" in MARFIS or ISDB
#' typically has multiple sets, and each set has its own coordinates.  For this
#' analysis, every set is analyzed to see which polygon it occurred in, and
#' this information is provided via as the values for \code{MARFIS_SETS} and
#' \code{ISDB_SETS} in the results.
#'
#' Coordinates for MARFIS_* data comes from the db table "PRO_SPC_INFO", and
#' coordinates for ISDB_*
#'  data come from "ISSETPROFILE" - using the first non-null
#' pair found while checking from P1-P4 in order.
#'
#' \item \code{_TRIPS} = Since a single trip can have sets across
#' multiple polygons, this script determines which polygon had the most sets for
#' a given trip, and defines that polygon as the value for a given trip.  These
#' are provided as the values for \code{MARFIS_TRIPS} and \code{ISDB_TRIPS}.
#'
#' \item Bad\code{Weird} Coordinates = If the latitude or longitude of the input data
#' for any set has a NULL value, an entry of "Bad coordinate" will be added to
#' the results.
#' Additionally, if fishing can occur outside of the extent of the provided polygon,
#' those trips/sets will be attributed to "Other".
#' }
#' @family simpleproducts
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
summarize_locations<-function(get_isdb = NULL,
                        get_marfis = NULL,
                        marfMatchOnly = TRUE,
                        agg.poly.shp = NULL,
                        agg.poly.field = NULL,
                        ...){
  args <- list(...)$args
  if (args$debuggit){
    Mar.utils::where_now()
    T_summarize_locations=Sys.time()
  }

  oTrips <- get_isdb$ISDB_TRIPS[!is.na(get_isdb$ISDB_TRIPS$TRIP_ID_MARF),]
  oSets <- get_isdb$ISDB_SETS[!is.na(get_isdb$ISDB_SETS$TRIP_ID_MARF),]

  if (marfMatchOnly){
    oTrips <- oTrips[!is.na(oTrips$TRIP_ID_MARF), ]
    oSets <- oSets[oSets$TRIP_ID %in% oTrips$TRIP_ID_ISDB,]
  }

  .I <- LOG_EFRT_STD_INFO_ID <- MON_DOC_ID<- cnt<- TRIP_ID <-NA

  if (is.null(agg.poly.shp)){
    agg.poly.shp=getExportedValue("Mar.data", args$areas)
    agg.poly.field = args$areasField

  }else{
    #use shapefile
    agg.poly.shp <- sf::st_read(agg.poly.shp)
  }

  if ("sf" %in% class(agg.poly.shp)){
    allAreas <- agg.poly.shp
    sf::st_geometry(allAreas) <- NULL
    allAreas = as.data.frame(allAreas[,agg.poly.field])

  }else if ("SpatialPolygons" %in% class(agg.poly.shp)){
    allAreas = as.data.frame(sort(agg.poly.shp@data[,agg.poly.field]))
  }
  colnames(allAreas)[1] <- agg.poly.field
  allAreas = rbind(allAreas, "Outside of Defined Areas")
  allAreas = rbind(allAreas, "Bad coordinate")
  #by set
  if (!args$quietly)message(paste0("\n", "Figuring out which area each set occurred in..."))
  if (!is.null(oSets) && nrow(oSets)>0){
    OBS_area_s = Mar.utils::identify_area(oSets,
                                          agg.poly.shp = agg.poly.shp,
                                          agg.poly.field = agg.poly.field)
  }else{
    OBS_area_s<- NA
  }
  #grab the first (valid) position from each set
  if (!is.null(get_marfis$MARF_SETS) && nrow(get_marfis$MARF_SETS)>0){
    MARF_sets_pos <- get_marfis$MARF_SETS[,c("MON_DOC_ID","TRIP_ID_MARF","LOG_EFRT_STD_INFO_ID","LATITUDE","LONGITUDE")]
    MARF_sets_pos <- MARF_sets_pos[with(MARF_sets_pos,order(LOG_EFRT_STD_INFO_ID)),]
    MARF_sets_pos <- MARF_sets_pos[!is.na(MARF_sets_pos$LATITUDE) & !is.na(MARF_sets_pos$LONGITUDE),]
    MARF_sets_pos <-data.table::as.data.table(MARF_sets_pos)
    MARF_sets_pos <- MARF_sets_pos[MARF_sets_pos[, .I[LOG_EFRT_STD_INFO_ID == min(LOG_EFRT_STD_INFO_ID)], by=MON_DOC_ID]$V1]
    MARF_sets_pos <- as.data.frame(MARF_sets_pos)
    MARF_sets_pos$LOG_EFRT_STD_INFO_ID <- NULL
    MARF_sets_pos <- unique(MARF_sets_pos[!is.na(MARF_sets_pos$MON_DOC_ID),])
    Marf_Trip_pos<-merge(get_marfis$MARF_TRIPS, MARF_sets_pos, all.x = T )
    MAR_area_s = Mar.utils::identify_area(get_marfis$MARF_SETS,
                                          agg.poly.shp = agg.poly.shp,
                                          agg.poly.field = agg.poly.field)
  }else{
    Marf_Trip_pos<-merge(get_marfis$MARF_TRIPS, MARF_sets_pos )
    MAR_area_s<- NA
  }

  #by_trip
  if (!args$quietly)message(paste0("\n", "Figuring out the area in which the most sets occurred during each trip.","\n"))

  if (!is.null(oTrips) && (!is.null(oSets) && nrow(oSets)>0)){
    O_trips = merge(oTrips[,!names(oTrips) %in% c("BOARD_DATE","LANDING_DATE")],
                    oSets, all.y =TRUE, by.x = "TRIP_ID_ISDB", by.y = "TRIP_ID")

    OBS_area_t <- Mar.utils::identify_area(O_trips,
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
  if(is.data.frame(MAR_area_t)){
    m_t = determineArea(df = MAR_area_t, setField = "TRIP_ID_MARF" , agg.poly.field = agg.poly.field, newID = "MARFIS_TRIPS")
    allAreas = merge(allAreas,m_t$summary, by= agg.poly.field, all.x=T)
    m_t = m_t$details
  }else{
    m_t<-NA
  }
  if(is.data.frame(OBS_area_t)){
    o_t = determineArea(df = OBS_area_t, setField = "TRIP_ID_ISDB" , agg.poly.field = agg.poly.field, newID = "ISDB_TRIPS")
    allAreas = merge(allAreas,o_t$summary, by= agg.poly.field, all.x=T)
    o_t = o_t$details
  }else{
    o_t<-NA
  }
  if(is.data.frame(MAR_area_s)){
    m_s = determineArea(df = MAR_area_s, setField = "LOG_EFRT_STD_INFO_ID" , agg.poly.field = agg.poly.field, newID = "MARFIS_SETS")
    allAreas = merge(allAreas,m_s$summary, by= agg.poly.field, all.x=T)
    m_s = m_s$details
  }else{
    m_s<-NA
  }
  if(is.data.frame(OBS_area_s)){
    o_s = determineArea(df = OBS_area_s, setField = "FISHSET_ID" , agg.poly.field = agg.poly.field, newID = "ISDB_SETS")
    allAreas = merge(allAreas,o_s$summary, by= agg.poly.field, all.x=T)
    o_s = o_s$details
  }else{
    o_s<-NA
  }
  #remove rows with no useful values
  allAreas =  unique(allAreas[rowSums(is.na(allAreas[,2:ncol(allAreas)]))!=ncol(allAreas)-1,])
  res = list()
  res[["summary"]]<-allAreas
  res[["details"]]<-list()
  res$details[["TRIPS_MARF"]] <- m_t
  res$details[["TRIPS_ISDB"]] <- o_t
  res$details[["SETS_MARF"]] <- m_s
  res$details[["SETS_ISDB"]] <- o_s
  if(exists("T_summarize_locations")) message("\n","summarize_locations() completed in",round( difftime(Sys.time(),T_summarize_locations,units = "secs"),0),"secs\n")
  return(res)
}
