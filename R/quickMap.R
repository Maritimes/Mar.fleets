#' @title quickMap
#' @description This function generates a simple leaflet plot for the output from Mar.bycatch
#' functions.
#' @param data  default is \code{NULL}. This is the entire output from any of the fleet wrappers.
#' @param vms default is \code{NULL}. This is optional, but can be the output from \code{get_vmstracks()}.
#' @param nafo  default is \code{TRUE}. This indicates whether or not the NAFO areas should be plotted.
#' @examples \dontrun{
#' redfishresults <- fleet_redfish(unit=3, year = "2017", useLocal=T,data.dir="c:/data/")
#' redfishVMS<-get_vmstracks(get_marfis = redfishresults$marf, get_isdb = redfishresults$isdb,
#'          useLocal=F, oracle.username = "me", oracle.password = "mypassword",
#'          oracle.dsn="PTRAN", usepkg='roracle')
#' quickMap(data=redfishresults, vms= redfishVMS, nafo=TRUE)
#'                                 }
#' @family simpleproducts
#' @return a leaflet map.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
quickMap <- function(data=NULL, vms= NULL, nafo=TRUE){
  bbLat <- NA
  bbLon <- NA
  overlayGroups <- NA

  ship =        leaflet::makeIcon("https://raw.githubusercontent.com/Rush/Font-Awesome-SVG-PNG/master/black/svg/ship.svg", 18, 18)
  obsOpen =     leaflet::makeIcon("https://raw.githubusercontent.com/Rush/Font-Awesome-SVG-PNG/master/black/svg/eye.svg", 18, 18)

  m <- leaflet::leaflet()
  m <- leaflet::addTiles(m)
  m <- leaflet::addWMSTiles(map = m,
                            group = "Basemap",
                            baseUrl = "https://services.arcgisonline.com/arcgis/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}.png",
                            layers = "1", options = leaflet::WMSTileOptions(format = "image/png", transparent = T))
  if (nafo) {

    m <- leaflet::addPolygons(group = "NAFO",
                              map = m, data = Mar.data::NAFOSubunits_sf, stroke = TRUE, color = "#666666", fill=F,
                              label=Mar.data::NAFOSubunits_sf$NAFO_BEST, weight = 1.5,
                              labelOptions = leaflet::labelOptions(noHide = T, textOnly = TRUE) )
    overlayGroups <- c(overlayGroups, "NAFO")
  }

  if (class(data$marf$MARF_SETS)=="data.frame"){
    commSets <- data$marf$MARF_SETS
    m <- leaflet::addMarkers(group = "MARFIS",
                             map = m, lng = commSets$LONGITUDE, lat = commSets$LATITUDE,
                             popup = paste0("MARFIS TRIP_ID:",commSets$TRIP_ID_MARF,
                                            "<br>PRO_SPC_INFO_ID", commSets$PRO_SPC_INFO_ID,
                                            "<br>LOG_EFRT_STD_INFO_ID",commSets$LOG_EFRT_STD_INFO_ID,
                                            "<br><br>RND_WEIGHT_KGS:", commSets$RND_WEIGHT_KGS),
                             icon = ship)

    overlayGroups <- c(overlayGroups, "MARFIS")
    bbLat <- c(bbLat, min(commSets$LATITUDE),max(commSets$LATITUDE))
    bbLon <- c(bbLon, min(commSets$LONGITUDE),max(commSets$LONGITUDE))
  }
  if (class(data$isdb$ISDB_SETS)=="data.frame"){
    isdbSets <- data$isdb$ISDB_SETS
    m <- leaflet::addMarkers(group = "ISDB",
                             map = m, lng = isdbSets$LONGITUDE , lat = isdbSets$LATITUDE,
                             popup = paste0("ISDB TRIP_ID:",isdbSets$TRIP_ID,
                                            "<br>FISHSET_ID", isdbSets$FISHSET_ID,
                                            "<br>LOG_EFRT_STD_INFO_ID",isdbSets$LOG_EFRT_STD_INFO_ID),
                             icon = obsOpen)
    overlayGroups <- c(overlayGroups, "ISDB")
    bbLat <- c(bbLat, min(isdbSets$LATITUDE), max(isdbSets$LATITUDE))
    bbLon <- c(bbLon, min(isdbSets$LONGITUDE), max(isdbSets$LONGITUDE))
  }

  if ("data.frame" %in% class(vms)){


    pal <- leaflet::colorFactor(
      palette = c('black','red'),
      domain = vms$OBS
    )
    m <- leaflet::addPolylines(group = "VMS",
                               map = m, data = vms, stroke = TRUE, color= ~pal(OBS),
                               weight = 2, popup = ifelse(vms$OBS == 0,"UNOBSERVED","OBSERVED"))
    overlayGroups <- c(overlayGroups, "VMS")
  }

  bbLat <- bbLat[!is.na(bbLat)]
  bbLon <- bbLon[!is.na(bbLon)]
  if(length(bbLat)>0 & length(bbLon)>0){
    m<- leaflet::fitBounds(map=m, lng1 = min(range(bbLon)), lng2 = max(range(bbLon)),
                           lat1 = max(range(bbLat)), lat2 = min(range(bbLat)))
  }
  overlayGroups <- overlayGroups[!is.na(overlayGroups)]
  m <- leaflet::addLayersControl(map=m, baseGroups = c("Basemap"), overlayGroups = overlayGroups,
                                 options = leaflet::layersControlOptions(collapsed = FALSE))

  return(m)
}

# makeMap <- function(data=NULL, VMS=NULL, sp = 30){
#
#   library(ggplot2)
#   data$isdb$ALL_ISDB_TRIPS <- data$isdb$ALL_ISDB_TRIPS[!is.na(data$isdb$ALL_ISDB_TRIPS$TRIP_ID_MARF),]
#   data$isdb$ALL_ISDB_SETS <- data$isdb$ALL_ISDB_SETS[!is.na(data$isdb$ALL_ISDB_SETS$TRIP_ID_MARF),]
#
#   nafosf<- Mar.data::NAFOSubunits_sf
#   basemap <- ggplot(data = nafosf)
#   basemap <- basemap + geom_sf(show.legend = FALSE)
#   basemap <- basemap + xlim(range(data$marf$MARF_SETS$LONGITUDE, na.rm = T)) + xlab("Lon")
#   basemap <- basemap + ylim(range(data$marf$MARF_SETS$LATITUDE, na.rm = T)) + ylab("Lat")
#   basemap <- basemap + geom_sf_text(aes(label = NAFO_BEST), size=3, show.legend = FALSE)
#
#
#   vmsColors = c("#7a7a7a", "#ff0000") #black, red
#   marfisdbSimpleVMS <- basemap + ggtitle(label = "marfis, isdb & VMS data", subtitle =paste0("observed catch of ", sp)) + geom_sf(data = VMS,aes(col=alpha(vmsColors[cut(OBS, 2)],0.2)))
#   marfisdbSimpleVMS <- marfisdbSimpleVMS + geom_point(data=data$marf$MARF_SETS,aes(x=LONGITUDE,y=LATITUDE, size=RND_WEIGHT_KGS),pch=19,alpha=0.25, color = "black")
#   marfisdbSimpleVMS <- marfisdbSimpleVMS + geom_point(data=data$isdb$ALL_ISDB_SETS,aes(x=LONGITUDE,y=LATITUDE, size=eval(parse(text = paste0("sp_",sp)))),pch=19,alpha=0.25,color = "red")
#   marfisdbSimpleVMS <- marfisdbSimpleVMS + scale_colour_identity(guide="legend",breaks=vmsColors)
#
#
#   return(marfisdbSimpleVMS)
#
# }
