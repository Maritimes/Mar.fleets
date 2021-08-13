#' @title quickMap
#' @description This function generates a simple leaflet plot for the output from Mar.bycatch
#' functions.
#' @param data  default is \code{NULL}. This is the entire output from any of the fleet wrappers.
#' @param plotMARF default is \code{TRUE}. Should MARFIS data be plotted? If there are more than
#' 1500 positions, the data will be displayed clustered.
#' @param showAllMARFSets default is \code{TRUE}. By default, the map will show all fishing
#' locations associated with the provided data.  If this is set to \code{FALSE}, only sets that
#' caught the specified \code{marfSpp} will be shown.  This can be useful for fleets that can retain
#' multiple species.
#' @param plotMARFSurf default is \code{FALSE}. If \code{TRUE}, an interpolated surface will be
#' generated for the MARFIS data. MARFIS point data will be interpolated using the "RND_WGT_KGS"
#' field.
#' @param marfSpp default is \code{NULL}.  If nothing is provided, the default directed species
#' will be pulled from the input data (e.g. if the data from fleet_halibut() is provided, halibut
#' (i.e. "130" will be used.))  Any  marfis species code(s) found in <data>$marf$MARF_CATCHES can be
#' used.
#' @param clusterMARF default is \code{TRUE}. If \code{TRUE}, MARF data will be grouped until
#' the map is zoomed in sufficiently  If \code{FALSE}, every MARF data point will be shown. If the
#' MARFIS data has > 1500 positions, it will be clustered regardless of this setting.
#' @param overloadMARF default is \code{"cluster"}. Valid values are 'cluster', and 'random'. This
#' map gets really slow when dealing with many positions, and this parameter indicates what should
#' be done with the MARFIS data if there are more sets than can reasonably be shown (i.e >1500).
#' \code{'cluster} causes the data to be shown as grouped symbols which expand into discrete points
#' as you zoom in.  \code{'random'} just grabs a random selection of 1500 points, and plots those.
#' @param plotISDB default is \code{TRUE}. Should ISDB data be plotted? If there are more than
#' 1500 positions, the data will be displayed clustered.
#' @param showAllISDBSets default is \code{TRUE}. By default, the map will show all of the ISDB set
#' locations associated with the provided data.  If this is set to \code{FALSE}, only sets that
#' caught the specified \code{isdbSpp} will be shown.  This can be useful for looking at the
#' locations of particular species.
#' @param plotISDBSurf default is \code{FALSE}. If \code{TRUE}, an interpolated surface will be
#' generated for the ISDB data. ISDB data contains several fields data for many species.  By default,
#' the interpolation will use the "EST_COMBINED_WT" field for the default directed species, but these
#' options can be overwritten by changing the values of \code{isdbField} and \code{isdbSpp},
#' respectively.
#' @param isdbSpp default is \code{NULL}.  If nothing is provided, the default directed species
#' will be pulled from the input data (e.g. if the data from fleet_halibut() is provided, halibut
#' (i.e. "30" will be used.)).  Any isdb species code(s) found in <data>$isdb$ISDB_CATCHES$ALL can
#' be used.  Entering \code{"?"} generates a user-selectable list of all of the available species.
#' @param clusterISDB default is \code{TRUE}. If \code{TRUE}, ISDB data will be grouped until
#' the map is zoomed in sufficiently  If \code{FALSE}, every ISDB data point will be shown.  If the
#' ISDB data has > 1500 positions, it will be clustered regardless of this setting.
#' @param overloadISDB default is \code{"cluster"}. Valid values are 'cluster', and 'random'. This
#' map gets really slow when dealing with many positions, and this parameter indicates what should
#' be done with the ISDB data if there are more sets than can reasonably be shown (i.e >1500).
#' \code{'cluster} causes the data to be shown as grouped symbols which expand into discrete points
#' as you zoom in.  \code{'random'} just grabs a random selection of 1500 points, and plots those.
#' @param isdbField default is \code{"EST_COMBINED_WT"}. Other valid values are "EST_NUM_CAUGHT",
#' "EST_KEPT_WT", and "EST_DISCARD_WT".
#' @param title default is \code{NULL}. This will be shown as the title of the map.
#' @param vms default is \code{NULL}. This is optional, but can be the output from \code{get_vmstracks()}.
#' If provided, VMS data will be plotted.
#' @param bathy default is \code{TRUE}. If \code{TRUE}, a bathymetry layer will be available.
#' @param surfRes default is \code{'low'}. This determines the resolution of any output surfaces.  Valid
#' values are 'low', 'med' or 'high'.  Higher values increase the time it takes to generate the surface.
#' @examples \dontrun{
#' redfishresults <- fleet_redfish(unit=3, year = "2017", useLocal=T,data.dir="c:/data/")
#' redfishVMS<-get_vmstracks(get_marfis = redfishresults$marf, get_isdb = redfishresults$isdb,
#'     useLocal=F, oracle.username = "me", oracle.password = "mypassword",
#'     oracle.dsn="PTRAN", usepkg='roracle')
#' quickMap(data=redfishresults, vms= redfishVMS)
#'        }
#' @family simpleproducts
#' @return a leaflet map.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
quickMap <- function(data=NULL,
                     plotMARF = TRUE, showAllMARFSets = TRUE, clusterMARF = TRUE, overloadMARF = "cluster", plotMARFSurf = FALSE, marfSpp = NULL,
                     plotISDB = TRUE, showAllISDBSets = TRUE, clusterISDB = TRUE, overloadISDB = "cluster", plotISDBSurf = FALSE, isdbField = "EST_COMBINED_WT", isdbSpp = NULL,
                     title = NULL, vms= NULL, bathy = TRUE, nafo=FALSE, surfRes = "low"){

  if (tolower(surfRes)=="med"){
    det = 10000
  }else if (tolower(surfRes)=="high"){
    det = 100000
  } else{
    det = 1000
  }

  compareValues <- function(s1, s2) {
    c1 <- unique(strsplit(s1, "")[[1]])
    c2 <- unique(strsplit(s2, "")[[1]])
    return(length(intersect(c1,c2))/length(c1))
    #as is, allows for c1 to be "5ZJ", while c2 is "5ZEJ" (ie all chars from 1 are in 2)
    #below would force identical
    # (length(intersect(c1,c2))/length(c1)+length(intersect(c1,c2))/length(c2))/2
  }

  if ((plotISDB | plotISDBSurf)) {

    if (!is.null(isdbSpp) && isdbSpp == "?"){
      isdbSppPickDone <- FALSE
      while (!isdbSppPickDone){
        availISDBSppCd <- unique(data$isdb$ISDB_CATCHES$SUMMARY[,c("SPEC", "COMMON", "SCI")])
        isdbSppPick <- utils::select.list(c("ALL",paste0(availISDBSppCd$COMMON, " (",availISDBSppCd$SPEC,")")),
                                          multiple=T, graphics=T,
                                          title='ISDB Spp?')
        if (length(isdbSppPick) == 0) {
          stop("ISDB spp selection cancelled.  Stopping")
        }else{
          isdbSppPickDone <- TRUE
        }
      }
      isdbSpp = sub(".*\\((.*)\\).*", "\\1", isdbSppPick)
    }

    if (is.null(isdbSpp)) {
      isdbSpp <- eval(parse(text=data$params$user[data$params$user$PARAMETER=="isdbSpp","VALUE"]))
    } else if (isdbSpp == "ALL") {
      isdbSpp <- unique(data$isdb$ISDB_CATCHES$SUMMARY$SPEC)
    }
    isdbSppComm <- paste0(SPECIES_ISDB[SPECIES_ISDB$SPECCD_ID %in% isdbSpp,"COMMON"], collapse = "_")
  }

  if ((plotMARF | plotMARFSurf)) {
    if (!is.null(marfSpp) && marfSpp == "?"){
      marfSppPickDone <- FALSE
      while (!marfSppPickDone){
        availMARFSppCd <- unique(data$marf$MARF_CATCHES$SPECIES_CODE)
        availMARFSppCd <- SPECIES_MARFIS[SPECIES_MARFIS$SPECIES_CODE %in% availMARFSppCd,c("SPECIES_CODE", "SPECIES_NAME")]
        marfSppPick <- utils::select.list(c("ALL",paste0(availMARFSppCd$SPECIES_NAME, " (",availMARFSppCd$SPECIES_CODE,")")),
                                          multiple=T, graphics=T,
                                          title='MARF Spp?')
        if (length(marfSppPick) == 0) {
          stop("MARF spp selection cancelled.  Stopping")
        }else{
          marfSppPickDone <- TRUE
        }
      }
      marfSpp = sub(".*\\((.*)\\).*", "\\1", marfSppPick)
    }
    if (is.null(marfSpp)) {
      marfSpp <- eval(parse(text=data$params$user[data$params$user$PARAMETER=="marfSpp","VALUE"]))
    }else if (marfSpp == "ALL"){
      marfSpp <- unique(data$marf$MARF_CATCHES$SPECIES_CODE)
    }
    marfSppComm <- paste0(SPECIES_MARFIS[SPECIES_MARFIS$SPECIES_CODE %in% marfSpp,"SPECIES_NAME"], collapse = "_")
  }
  bbLat <- NA
  bbLon <- NA
  overlayGroups <- NA
  clustMARF <- NULL
  clustISDB <- NULL
  clustLimit <- 1500
  surfCols <- c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")
  m <- leaflet::leaflet()
  m <- leaflet::addTiles(map = m)


  baseGroups <- "None"
  if (bathy){
    m <- leaflet::addWMSTiles(map = m,
                              group = "Bathymetry",
                              baseUrl = "https://services.arcgisonline.com/arcgis/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}.png",
                              layers = "1", options = leaflet::WMSTileOptions(format = "image/png", transparent = T))
    baseGroups <-  c(baseGroups, c("Bathymetry"))
  }


  bonusLayer <- gsub('"',"",data$params$user[data$params$user$PARAMETER=="areaFile","VALUE"])
  bonusField <- gsub('"',"",data$params$user[data$params$user$PARAMETER=="areaFileField","VALUE"])
  if (bonusLayer != "NAFOSubunits_sf" | bonusField != "NAFO_1"){
    theData     <-   eval(parse(text=paste0("Mar.data::",bonusLayer)))
    theData <- theData[!is.na(theData[[bonusField]]),]
    theData[[bonusField]] <-  as.factor(theData[[bonusField]])
    factpal <- leaflet::colorFactor(palette = "viridis", theData[[bonusField]])
    bonusLayerCln <- gsub(x = bonusLayer, pattern = "_sf",replacement = "")
    m <- leaflet::addPolygons(group = bonusLayerCln,
                              map = m, data = theData, stroke = T, smoothFactor = 0, fillOpacity = 0.5,
                              color = factpal(theData[[bonusField]]),
                              label=theData[[bonusField]], weight = 1.5,
                              labelOptions = leaflet::labelOptions(noHide = F, textOnly = TRUE, textsize = 0.2)
    )
    overlayGroups <- c(overlayGroups, bonusLayerCln)
  }
  m <- leaflet::addPolygons(group = "NAFO",
                            map = m, data = Mar.data::NAFOSubunits_sf, stroke = TRUE, color = "#666666", fill=T,
                            label=Mar.data::NAFOSubunits_sf$NAFO_BEST, weight = 0.4,
                            labelOptions = leaflet::labelOptions(noHide = T, textOnly = TRUE, textsize = 0.2,
                                                                 style = list(
                                                                   "color" = "rgba(0,0,0,0.55)")))
  overlayGroups <- c(overlayGroups, "NAFO")
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
    legendHtml <- "<div id='legend', style='display:none; padding: 10px; padding-bottom: 10px;'>Legend"
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
  ship_coord <-   leaflet::makeAwesomeIcon(icon = "ship", markerColor = "pink", iconColor = "black", library = "fa")
  isdb_OBS <- leaflet::makeAwesomeIcon(icon = "eye", markerColor = "blue", iconColor = "black", library = "fa")
  isdb_OBS_coord <- leaflet::makeAwesomeIcon(icon = "eye", markerColor = "pink", iconColor = "black", library = "fa")
  isdb_Log <- leaflet::makeAwesomeIcon(icon = "eye-slash", markerColor = "darkblue", iconColor = "black", library = "fa")
  isdb_Log_coord <- leaflet::makeAwesomeIcon(icon = "eye-slash", markerColor = "pink", iconColor = "black", library = "fa")
  iconSet  <- leaflet::awesomeIconList(
    MARFIS = ship,
    MARFIS_coord_issue = ship_coord,
    ISDB_OBS = isdb_OBS,
    ISDB_OBS_coord_issue = isdb_OBS_coord,
    ISDB_LOG = isdb_Log,
    ISDB_LOG_coord_issue = isdb_Log_coord
  )

  if(!plotMARF) {
    iconSet$MARFIS <- NULL
    iconSet$MARFIS_coord_issue <- NULL
  }

  if(!plotISDB) {
    iconSet$ISDB_OBS <- NULL
    iconSet$ISDB_Log <- NULL
    iconSet$ISDB_OBS_coord_issue <- NULL
    iconSet$ISDB_Log_coord_issue <- NULL
  }

  if ((plotMARF | plotMARFSurf)  & class(data$marf$MARF_SETS)=="data.frame"){
    commSets <- Mar.utils::df_qc_spatial(data$marf$MARF_SETS)
    if (showAllMARFSets){
      theseCatM <- data$marf$MARF_CATCHES[,c("TRIP_ID_MARF", "LOG_EFRT_STD_INFO_ID", "SPECIES_CODE","RND_WEIGHT_KGS")]
    }else{
      theseCatM <- data$marf$MARF_CATCHES[data$marf$MARF_CATCHES$SPECIES_CODE %in% marfSpp,c("TRIP_ID_MARF", "LOG_EFRT_STD_INFO_ID", "SPECIES_CODE","RND_WEIGHT_KGS")]
    }
    theseCatM <- merge(theseCatM, SPECIES_MARFIS[, c("SPECIES_CODE","SPECIES_NAME")])
    theseCatM$SPECIES_CODE <- NULL
    theseCatM_sp <- stats::aggregate(by=theseCatM[c("TRIP_ID_MARF", "LOG_EFRT_STD_INFO_ID")], x = theseCatM[c("SPECIES_NAME")], paste, collapse = "</dd><dd>")
    theseCatM_sp$SPECIES_NAME <- paste0("<dd>",theseCatM_sp$SPECIES_NAME,"</dd>")

    theseCatM_sum <- stats::aggregate(by=theseCatM[c("TRIP_ID_MARF", "LOG_EFRT_STD_INFO_ID")], x = theseCatM[c("RND_WEIGHT_KGS")], sum)
    theseCatM <- merge(theseCatM_sum, theseCatM_sp)
    colnames(theseCatM)[colnames(theseCatM)=="SPECIES_NAME"] <- "spp"

    if (showAllMARFSets) {
      commSets <- merge(commSets, theseCatM, all.x=T)
    }else{
      commSets <- merge(commSets, theseCatM)
    }

    commSets$icon <- "MARFIS"

    commSets$coordchk <- mapply(compareValues, commSets$NAFO_MARF_SETS,commSets$NAFO_MARF_SETS_CALC)
    commSets[which(commSets$coordchk != 1),"icon"] <- "MARFIS_coord_issue"
    commSets$coordchk <- NULL

    if (plotMARFSurf){
      marfSurfDat <- stats::aggregate(
        x = list(RND_WEIGHT_KGS = commSets$RND_WEIGHT_KGS),
        by = list(LATITUDE = commSets$LATITUDE,
                  LONGITUDE = commSets$LONGITUDE),
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
      if (nrow(commSets)>clustLimit) message(paste0("The marf data has too many results to be shown on the map (i.e. > ", clustLimit,"). The choice indicated by overloadMARF ('",overloadMARF,"') will be imposed."))
      if ((nrow(commSets)>clustLimit & overloadMARF == "cluster") | clusterMARF){
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
      if (nrow(commSets)>clustLimit & overloadMARF != "cluster") commSets <- commSets[sample.int(nrow(commSets), clustLimit),]

      m <- leaflet::addAwesomeMarkers(map = m, group = "MARFIS", data = commSets, lng = commSets$LONGITUDE, lat = commSets$LATITUDE, icon = ~iconSet[icon], clusterOptions = clustMARF,
                                      popup = paste0("MARFIS TRIP_ID:", commSets$TRIP_ID_MARF,
                                                     "<br>PRO_SPC_INFO_ID: ", commSets$PRO_SPC_INFO_ID,
                                                     "<br>LOG_EFRT_STD_INFO_ID: ", commSets$LOG_EFRT_STD_INFO_ID,
                                                     "<br>SPP: ", commSets$spp,
                                                     "<br>RND_WEIGHT_KGS: ", commSets$RND_WEIGHT_KGS," kgs <br>(combined wt of all spp above)",
                                                     ifelse(commSets$NAFO_MARF_SETS != commSets$NAFO_MARF_SETS_CALC,paste0("<br><br>Reported NAFO: ", commSets$NAFO_MARF_SETS,
                                                                                                                           "<br>Calculated NAFO: ", commSets$NAFO_MARF_SETS_CALC),""))
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
    message(nrow(data$isdb$ISDB_SETS)-nrow(isdbSets), " ISDB positions had bad coordinates and couldn't be used")
    if (showAllISDBSets) {
      theseCat <- data$isdb$ISDB_CATCHES$ALL[,c("TRIP_ID", "FISHSET_ID", "SPECCD_ID", isdbField)]
    }else{
      theseCat <- data$isdb$ISDB_CATCHES$ALL[data$isdb$ISDB_CATCHES$ALL$SPECCD_ID %in% isdbSpp,c("TRIP_ID", "FISHSET_ID", "SPECCD_ID", isdbField)]
    }
    theseCat <- merge(theseCat, SPECIES_ISDB[, c("SPECCD_ID","COMMON")])
    theseCat$SPECIES_CODE <- NULL
    theseCat_sp <- stats::aggregate(by=theseCat[c("TRIP_ID", "FISHSET_ID")], x = theseCat[c("COMMON")], paste, collapse = "</dd><dd>")
    theseCat_sp$COMMON <- paste0("<dd>",theseCat_sp$COMMON,"</dd>")
    theseCat_sum <- stats::aggregate(by=theseCat[c("TRIP_ID", "FISHSET_ID")], x = theseCat[isdbField], sum)
    theseCat <- merge(theseCat_sum, theseCat_sp)

    colnames(theseCat)[colnames(theseCat)=="get(isdbField)"] <- isdbField
    colnames(theseCat)[colnames(theseCat)=="COMMON"] <- "spp"

    if (showAllISDBSets) {
      isdbSets <- merge(isdbSets, theseCat, all.x=T)
    }else{
      isdbSets <- merge(isdbSets, theseCat)
    }

    isdbSets[["NAFO_ISDB_SETS"]][is.na(isdbSets[["NAFO_ISDB_SETS"]])] <- -9
    isdbSets[["NAFO_ISDB_SETS_CALC"]][is.na(isdbSets[["NAFO_ISDB_SETS_CALC"]])] <- -8
    isdbSets$icon <- NA
    isdbSets$coordchk <- mapply(compareValues, isdbSets$NAFO_ISDB_SETS,isdbSets$NAFO_ISDB_SETS_CALC)
    isdbSets[which(isdbSets$SOURCE ==0 & isdbSets$coordchk < 1 ),"icon"] <- "ISDB_OBS_coord_issue"
    isdbSets[which(isdbSets$SOURCE ==0 & isdbSets$coordchk == 1),"icon"] <- "ISDB_OBS"
    isdbSets[which(isdbSets$SOURCE ==1 & isdbSets$coordchk < 1 ),"icon"] <- "ISDB_LOG_coord_issue"
    isdbSets[which(isdbSets$SOURCE ==1 & isdbSets$coordchk == 1 ),"icon"] <- "ISDB_LOG"
    isdbSets$coordchk <- NULL


    if (plotISDBSurf){
      # theseCat <- data$isdb$ISDB_CATCHES$ALL[data$isdb$ISDB_CATCHES$ALL$SPECCD_ID %in% isdbSpp,c("TRIP_ID", "FISHSET_ID", isdbField)]
      # isdbSurfDat <- merge(isdbSets[,c("LATITUDE","LONGITUDE","TRIP_ID", "FISHSET_ID")], theseCat)
      isdbSurfDat <- stats::aggregate(
        x = list(aggField = isdbSets[isdbField]),
        by = list(LATITUDE = isdbSets$LATITUDE,
                  LONGITUDE = isdbSets$LONGITUDE
        ),
        sum
      )

      isdbSurf = makeSurface(data = isdbSurfDat[,c("LATITUDE","LONGITUDE",isdbField)])
      groupname = paste0("ISDB_surf_",isdbSppComm)
      palSurf2 <- leaflet::colorNumeric(surfCols, raster::values(isdbSurf), na.color = "transparent")
      m = leaflet::addRasterImage(map=m, group=groupname, isdbSurf, colors = palSurf2, opacity = 1)
      overlayGroups <- c(overlayGroups, groupname)
      extI<-raster::extent(isdbSurf)
      bbLat <- c(bbLat, extI@ymin, extI@ymax)
      bbLon <- c(bbLon, extI@xmin, extI@xmax)
    }

    if (plotISDB & nrow(isdbSets)>0){
      if (nrow(isdbSets)>clustLimit) message(paste0("The ISDB data has too many results to be shown on the map (i.e. > ", clustLimit,"). The choice indicated by overloadISDB ('",overloadISDB,"') will be imposed."))
      if ((nrow(isdbSets)>clustLimit & overloadISDB == "cluster") | clusterISDB){
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
      if (nrow(isdbSets)>clustLimit & overloadISDB != "cluster") isdbSets <- isdbSets[sample.int(nrow(isdbSets), clustLimit),]

      isdbunits <- "kgs<br>(combined wt of all spp above)"
      if (!grepl(pattern = "_wt", x = isdbField, ignore.case = T)) isdbunits <- "<br>(combined count of all spp above)"

      m <- leaflet::addAwesomeMarkers(map = m, group = "ISDB", data=isdbSets,lng = isdbSets$LONGITUDE , lat = isdbSets$LATITUDE, icon =  ~iconSet[icon], clusterOptions = clustISDB,
                                      popup = paste0("ISDB TRIP_ID: ",isdbSets$TRIP_ID,
                                                     "<br>FISHSET_ID: ", isdbSets$FISHSET_ID,
                                                     "<br>LOG_EFRT_STD_INFO_ID: ",isdbSets$LOG_EFRT_STD_INFO_ID,
                                                     "<br>SOURCE: ",isdbSets$SOURCE,
                                                     "<br>SPP: ", isdbSets$spp,
                                                     "<br>",toupper(isdbField),": ", isdbSets[,isdbField]," ", isdbunits,
                                                     ifelse(isdbSets$NAFO_ISDB_SETS != isdbSets$NAFO_ISDB_SETS_CALC,paste0("<br><br>Reported NAFO: ", isdbSets$NAFO_ISDB_SETS,
                                                                                                                           "<br>Calculated NAFO: ", isdbSets$NAFO_ISDB_SETS_CALC),""))
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
    vmsObs <- vms[vms$OBS==1,]
    vmsUnObs <- vms[vms$OBS!=1,]

    m <- leaflet::addPolylines(map = m, group = "VMS_no_observer", data = vmsUnObs, stroke = TRUE, color= "#666666", weight = 1.5,
                               label=~paste0("VR: ",VR_NUMBER),
                               labelOptions = leaflet::labelOptions(noHide = F, textOnly = TRUE, textsize = 0.2,
                                                                    style = list("color" = "black")),
                               popup = ~paste0("NO OBSERVER on board
                                                <br><dd>VR_NUMBER: ",VR_NUMBER,
                                               "<br><dd>trekMin:",trekMin,
                                               "<br><dd>trekMax:",trekMax))
    m <- leaflet::addPolylines(map = m, group = "VMS_observer", data = vmsObs, stroke = TRUE, color= "red", weight = 1.5,
                               label=~paste0("VR: ",VR_NUMBER),
                               labelOptions = leaflet::labelOptions(noHide = F, textOnly = TRUE, textsize = 0.2,
                                                                    style = list("color" = "mahogany")),
                               popup = ~paste0("OBSERVER present
                                                <br><dd>VR_NUMBER: ",VR_NUMBER,
                                               "<br><dd>trekMin:",trekMin,
                                               "<br><dd>trekMax:",trekMax))
    overlayGroups <- c(overlayGroups, "VMS_observer", "VMS_no_observer")
  }

  bbLat <- bbLat[!is.na(bbLat)]
  bbLon <- bbLon[!is.na(bbLon)]
  if(length(bbLat)>0 & length(bbLon)>0){
    m<- leaflet::fitBounds(map=m, lng1 = min(range(bbLon)), lng2 = max(range(bbLon)),
                           lat1 = max(range(bbLat)), lat2 = min(range(bbLat)))
  }
  overlayGroups <- overlayGroups[!is.na(overlayGroups)]
  m <- leaflet::addLayersControl(map=m, baseGroups = baseGroups, overlayGroups = overlayGroups, options = leaflet::layersControlOptions(collapsed = TRUE))
  if (!is.null(title)) m <- leaflet::addControl(map=m, html = titleHTML, position = "bottomleft")
  m <- leaflet::addEasyButton(map = m, leaflet::easyButton(position = "topright",
                                                           icon = htmltools::span(class = "star", htmltools::HTML("&starf;")),
                                                           onClick = leaflet::JS("
    function(btn, map){
      var x = document.getElementById('legend');
      if (x.style.display === 'none') {
        x.style.display ='block';
      } else {
        x.style.display = 'none';
      }
    }
    ")))
  if (plotMARF | plotISDB) m <- leaflet::addControl(map=m, html = markerLegendHTML(IconSet = iconSet), position = "topright")
  m <- leaflet::hideGroup(map=m, group = "MARFIS")
  m <- leaflet::hideGroup(map=m, group = "ISDB")
  m <- leaflet::hideGroup(map=m, group = "NAFO")
  m <- leaflet::hideGroup(map=m, group = bonusLayerCln)
  return(m)
}
