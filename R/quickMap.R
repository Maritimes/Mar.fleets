#' @title quickMap
#' @description This function generates a simple leaflet plot for the output from Mar.bycatch
#' functions.
#' @param data  default is \code{NULL}. This is the entire output from any of the fleet wrappers.
#' @param title default is \code{NULL}. This will be shown as the title of the map.
#' @param plotMARF default is \code{TRUE}. Should MARFIS data be plotted?
#' @param plotISDB default is \code{TRUE}. Should ISDB data be plotted?
#' @param cluster default is \code{TRUE}. If \code{TRUE}, MARF and ISDB data will be grouped until
#' the map is zoomed in sufficiently  If \code{FALSE}, every MARF and/or ISDB data point will be shown.
#' @param plotMARFSurf default is \code{FALSE}. If \code{TRUE}, an interpolated surface will be generated
#' for the MARFIS data. MARFIS point data will be interpolated using the "RND_WGT_KGS" field.
#' @param plotISDBSurf default is \code{FALSE}. If \code{TRUE}, an interpolated surface will be generated
#' for the ISDB data. ISDB data contains several fields data for many species.  By default, the
#'  interpolation will use the "EST_COMBINED_WT" field for the default directed species, but these
#'  options can be overwritten by changing the values of \code{isdbSurfField} and \code{isdbSurfSpp},
#'  respectively.
#' @param isdbSurfField default is \code{"EST_COMBINED_WT"}.  Other valid values are "EST_NUM_CAUGHT" and "EST_DISCARD_WT".
#' @param marfSurfSpp default is \code{NULL}.  If nothing is provided, the default directed species
#' will be pulled from the input data (e.g. if the data from fleet_halibut() is provided, halibut
#' (i.e. "130" will be used.))  Any  marfis species code(s) found in <data>$marf$ISDB_CATCHES can be used.
#' @param isdbSurfSpp default is \code{NULL}.  If nothing is provided, the default directed species
#' will be pulled from the input data (e.g. if the data from fleet_halibut() is provided, halibut
#' (i.e. "30" will be used.))  Any  isdb species code(s) found in <data>$isdb$ISDB_CATCHES$ALL can be used.
#' @param vms default is \code{NULL}. This is optional, but can be the output from \code{get_vmstracks()}.
#' If provided, VMS data will be plotted.
#' @param bathy default is \code{TRUE}. If \code{TRUE}, a bathymetry layer will be available.
#' @param nafo  default is \code{TRUE}. This indicates whether or not the NAFO areas should be plotted.
#' @param surfRes default is \code{'low'}. This determines the resolution of any output surfaces.  Valid
#' values are 'low', 'med' or 'high'.  Higher values increase the time it takes to generate the surface.
#' @examples \dontrun{
#' redfishresults <- fleet_redfish(unit=3, year = "2017", useLocal=T,data.dir="c:/data/")
#' redfishVMS<-get_vmstracks(get_marfis = redfishresults$marf, get_isdb = redfishresults$isdb,
#'     useLocal=F, oracle.username = "me", oracle.password = "mypassword",
#'     oracle.dsn="PTRAN", usepkg='roracle')
#' quickMap(data=redfishresults, vms= redfishVMS, nafo=TRUE)
#'        }
#' @family simpleproducts
#' @return a leaflet map.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
quickMap <- function(data=NULL, title = NULL, plotMARF = TRUE, plotISDB = TRUE, cluster = TRUE, plotMARFSurf = FALSE, plotISDBSurf = FALSE, isdbSurfField = "EST_COMBINED_WT", isdbSurfSpp = NULL, marfSurfSpp = NULL, vms= NULL, bathy = TRUE, nafo=TRUE, surfRes = "low"){

  if (tolower(surfRes)=="med"){
    det = 10000
  }else if (tolower(surfRes)=="high"){
    det = 100000
  } else{
    det = 1000
  }



  if ((plotISDB | plotISDBSurf)) {
    if (is.null(isdbSurfSpp)) isdbSurfSpp <- data$params$user[data$params$user$PARAMETER=="isdbSpp","VALUE"]
    isdbSppComm <- paste0(SPECIES_ISDB[SPECIES_ISDB$SPECCD_ID == isdbSurfSpp,"COMMON"], collapse = "_")
  }

  if ((plotMARF | plotMARFSurf)) {
    if (is.null(marfSurfSpp)) marfSurfSpp <- data$params$user[data$params$user$PARAMETER=="marfSpp","VALUE"]
    marfSppComm <- paste0(SPECIES_MARFIS[SPECIES_MARFIS$SPECIES_CODE == marfSurfSpp,"SPECIES_NAME"], collapse = "_")
  }
  bbLat <- NA
  bbLon <- NA
  overlayGroups <- NA
  clustMARF <- NULL
  clustISDB <- NULL
  surfCols <- c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")
  m <- leaflet::leaflet()
  m <- leaflet::addTiles(m)
  baseGroups <- "None"
  if (bathy){
    m <- leaflet::addWMSTiles(map = m,
                              group = "Bathymetry",
                              baseUrl = "https://services.arcgisonline.com/arcgis/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}.png",
                              layers = "1", options = leaflet::WMSTileOptions(format = "image/png", transparent = T))
    baseGroups <-  c(baseGroups, c("Bathymetry"))
  }

  if (nafo) {
    m <- leaflet::addPolygons(group = "NAFO",
                              map = m, data = Mar.data::NAFOSubunits_sf, stroke = TRUE, color = "#666666", fill=T,
                              label=Mar.data::NAFOSubunits_sf$NAFO_BEST, weight = 1.5,
                              labelOptions = leaflet::labelOptions(noHide = F, textOnly = TRUE) )
    overlayGroups <- c(overlayGroups, "NAFO")
  }
  titleHTML <- paste0("<div style='
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;'>",title,"</div>")
  makeSurface <- function(data=NULL, det= det){
    #just set data to LAT, LONG and <interpfield>, in that order
    data_sp <- Mar.utils::df_to_sp(data)
    grd    <- as.data.frame(sp::spsample(data_sp, "regular", n=det))
    names(grd)  <- c("X", "Y")
    sp::coordinates(grd) <- c("X", "Y")
    sp::gridded(grd)     <- TRUE
    sp::fullgrid(grd)    <- TRUE
    sp::proj4string(grd) <- suppressWarnings(sp::proj4string(data_sp))
    surf <- gstat::idw(eval(parse(text = names(data_sp@data)[3]))~1, data_sp, newdata=grd, idp=3.0)
    surf <- raster::raster(surf)
    landMask <- Mar.data::NAFOSubunitsLnd[Mar.data::NAFOSubunitsLnd@data$NAFO_BEST != "<LAND>", ]
    surf.m     <- raster::mask(surf, landMask)
    return(surf.m)
  }

  markerLegendHTML <- function(IconSet) {
    legendHtml <- "<div style='padding: 10px; padding-bottom: 10px;'>"
    n <- 1
    for (Icon in IconSet) {
      if (Icon[["library"]] == "fa") {
        legendHtml<- paste0(legendHtml, "<div style='width: auto; height: 45px'>",
                            "<div style='position: relative; display: inline-block; width: 36px; height: 45px' class='awesome-marker-icon-",Icon[["markerColor"]]," awesome-marker'>",
                            "<i style='margin-left: 4px; margin-top: 11px; 'class= 'fa fa-",Icon[["icon"]]," fa-inverse'></i>",
                            "</div>",
                            "<p style='font-size: 12px; position: relative; top: 10px; display: inline-block; ' >", names(IconSet)[n] ,"</p>",
                            "</div>")
      }
      n<- n + 1
    }
    paste0(legendHtml, "</div>")
  }

  ship <-     leaflet::makeAwesomeIcon(icon = "ship", markerColor = "darkred", iconColor = "black", library = "fa")
  isdb_OBS <- leaflet::makeAwesomeIcon(icon = "eye", markerColor = "blue", iconColor = "black", library = "fa")
  isdb_Log <- leaflet::makeAwesomeIcon(icon = "eye-slash", markerColor = "darkblue", iconColor = "black", library = "fa")
  iconSet  <- leaflet::awesomeIconList(
    MARFIS = ship,
    ISDB_OBS = isdb_OBS,
    ISDB_Log = isdb_Log
  )

  if(!plotMARF) iconSet$commSets <- NULL

  if(!plotISDB) {
    iconSet$ISDB_OBS <- NULL
    iconSet$ISDB_Log <- NULL
  }

  if ((plotMARF | plotMARFSurf)  & class(data$marf$MARF_SETS)=="data.frame"){
    commSets <- Mar.utils::df_qc_spatial(data$marf$MARF_SETS)
    message(nrow(data$marf$MARF_SETS)-nrow(commSets), " MARF positions had bad coordinates and couldn't be used")

    if (plotMARFSurf){
      theseCat <- data$marf$MARF_CATCHES[data$marf$MARF_CATCHES$SPECIES_CODE %in% marfSurfSpp,c("TRIP_ID_MARF", "LOG_EFRT_STD_INFO_ID", "RND_WEIGHT_KGS")]
      marfSurfDat <- merge(commSets[,c("LATITUDE","LONGITUDE","LOG_EFRT_STD_INFO_ID")], theseCat)
      marfSurfDat <- stats::aggregate(
        x = list(RND_WEIGHT_KGS = marfSurfDat$RND_WEIGHT_KGS),
        by = list(LATITUDE = marfSurfDat$LATITUDE,
                  LONGITUDE = marfSurfDat$LONGITUDE
        ),
        sum
      )

      marfSurf = makeSurface(data = commSets[,c("LATITUDE","LONGITUDE","RND_WEIGHT_KGS")])
      groupname = paste0("MARFIS_surf_",marfSppComm)
      palSurf <- leaflet::colorNumeric(surfCols, raster::values(marfSurf), na.color = "transparent")
      m = leaflet::addRasterImage(map=m, group=groupname, marfSurf, colors = palSurf, opacity = 1)
      overlayGroups <- c(overlayGroups, groupname)
      extM<-raster::extent(marfSurf)
      bbLat <- c(bbLat, extM@ymin, extM@ymax)
      bbLon <- c(bbLon, extM@xmin, extM@xmax)
    }

    if (plotMARF & nrow(commSets)>0){
      if (cluster) {
        clustMARF = leaflet::markerClusterOptions(iconCreateFunction=leaflet::JS("
        function (cluster) {
          var childCount = cluster.getChildCount();
          if (childCount < 100) {
            c = 'rgba(254,217,118, 1.0);'
          } else if (childCount < 1000) {
            c = 'rgba(253,141,60, 1);'
          } else {
            c = 'rgba(189,0,38, 1);'
          }
            return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
          }
      "))
      }
      m <- leaflet::addAwesomeMarkers(map = m, group = "MARFIS", data = commSets, lng = commSets$LONGITUDE, lat = commSets$LATITUDE, icon = ship, clusterOptions = clustMARF,
                                      popup = paste0("MARFIS TRIP_ID:", commSets$TRIP_ID_MARF,
                                                     "<br>PRO_SPC_INFO_ID: ", commSets$PRO_SPC_INFO_ID,
                                                     "<br>LOG_EFRT_STD_INFO_ID: ", commSets$LOG_EFRT_STD_INFO_ID,
                                                     "<br><br>RND_WEIGHT_KGS: ", commSets$RND_WEIGHT_KGS)
      )
      overlayGroups <- c(overlayGroups, "MARFIS")
      bbLat <- c(bbLat, min(commSets$LATITUDE, na.rm = T),max(commSets$LATITUDE, na.rm = T))
      bbLon <- c(bbLon, min(commSets$LONGITUDE, na.rm = T),max(commSets$LONGITUDE, na.rm = T))
    }
  }else{
    commSets <- NA
  }
  if ((plotISDB | plotISDBSurf) & class(data$isdb$ISDB_SETS)=="data.frame"){
    isdbSets <- Mar.utils::df_qc_spatial(data$isdb$ISDB_SETS)
    isdbSets$icon<- ifelse(isdbSets$SOURCE ==0, "ISDB_OBS", "ISDB_Log")
    message(nrow(data$isdb$ISDB_SETS)-nrow(isdbSets), " ISDB positions had no coordinates and couldn't be used")

    if (plotISDBSurf){
      theseCat <- data$isdb$ISDB_CATCHES$ALL[data$isdb$ISDB_CATCHES$ALL$SPECCD_ID %in% isdbSurfSpp,c("TRIP_ID", "FISHSET_ID", isdbSurfField)]
      isdbSurfDat <- merge(isdbSets[,c("LATITUDE","LONGITUDE","TRIP_ID", "FISHSET_ID")], theseCat)
      isdbSurfDat <- stats::aggregate(
        x = list(aggField = isdbSurfDat[isdbSurfField]),
        by = list(LATITUDE = isdbSurfDat$LATITUDE,
                  LONGITUDE = isdbSurfDat$LONGITUDE
        ),
        sum
      )

      isdbSurf = makeSurface(data = isdbSurfDat[,c("LATITUDE","LONGITUDE",isdbSurfField)])
      groupname = paste0("ISDB_surf_",isdbSppComm)
      palSurf2 <- leaflet::colorNumeric(surfCols, raster::values(isdbSurf), na.color = "transparent")
      m = leaflet::addRasterImage(map=m, group=groupname, isdbSurf, colors = palSurf2, opacity = 1)
      overlayGroups <- c(overlayGroups, groupname)
      extI<-raster::extent(isdbSurf)
      bbLat <- c(bbLat, extI@ymin, extI@ymax)
      bbLon <- c(bbLon, extI@xmin, extI@xmax)
    }

    if (plotISDB & nrow(isdbSets)>0){


      if (cluster){
        clustISDB = leaflet::markerClusterOptions(iconCreateFunction=leaflet::JS("
      function (cluster) {
        var childCount = cluster.getChildCount();
        if (childCount < 500) {
          c = 'rgba(107,174,214,1);'
        } else if (childCount < 2000) {
          c = 'rgba(49,130,189, 1);'
        } else {
          c = 'rgba(8,81,156, 1);'
        }
          return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
        }
      "))
      }
      m <- leaflet::addAwesomeMarkers(map = m, group = "ISDB", data=isdbSets,lng = isdbSets$LONGITUDE , lat = isdbSets$LATITUDE, icon =  ~iconSet[icon], clusterOptions = clustISDB,
                                      popup = paste0("ISDB TRIP_ID: ",isdbSets$TRIP_ID,
                                                     "<br>FISHSET_ID: ", isdbSets$FISHSET_ID,
                                                     "<br>LOG_EFRT_STD_INFO_ID: ",isdbSets$LOG_EFRT_STD_INFO_ID,
                                                     "<br>SOURCE: ",isdbSets$SOURCE)
      )

      overlayGroups <- c(overlayGroups, "ISDB")
      bbLat <- c(bbLat, min(isdbSets$LATITUDE, na.rm = T), max(isdbSets$LATITUDE, na.rm = T))
      bbLon <- c(bbLon, min(isdbSets$LONGITUDE, na.rm = T), max(isdbSets$LONGITUDE, na.rm = T))
      overlayGroups <- c(overlayGroups, "ISDB")
    }

  }else{
    isdbSets <- NA
  }

  if ("data.frame" %in% class(vms)){
    pal <- leaflet::colorFactor(
      palette = c('black','red'),
      domain = vms$OBS
    )
    m <- leaflet::addPolylines(map = m, group = "VMS", data = vms, stroke = TRUE, color= ~pal(OBS), weight = 2, popup = ifelse(vms$OBS == 0,"UNOBSERVED","OBSERVED"))
    overlayGroups <- c(overlayGroups, "VMS")
  }

  bbLat <- bbLat[!is.na(bbLat)]
  bbLon <- bbLon[!is.na(bbLon)]
  if(length(bbLat)>0 & length(bbLon)>0){
    m<- leaflet::fitBounds(map=m, lng1 = min(range(bbLon)), lng2 = max(range(bbLon)),
                           lat1 = max(range(bbLat)), lat2 = min(range(bbLat)))
  }
  overlayGroups <- overlayGroups[!is.na(overlayGroups)]
  m <- leaflet::addLayersControl(map=m, baseGroups = baseGroups, overlayGroups = overlayGroups, options = leaflet::layersControlOptions(collapsed = TRUE))
  m <- leaflet::addControl(map=m, html = titleHTML, position = "bottomleft")
  m <- leaflet::hideGroup(map=m, group = "MARFIS")
  m <- leaflet::hideGroup(map=m, group = "ISDB")
  return(m)
}
