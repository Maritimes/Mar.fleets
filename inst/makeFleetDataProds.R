# devtools::install_github("Maritimes/Mar.bycatch")
# library(Mar.bycatch)
library(Mar.datawrangling)
library(Mar.utils)
makeFleetDataProds <- function(binYears = 5, startYr = 2002, data.dir ="C:/git/wrangledData/", saveDir =NULL, fleet = NULL){

  startTime1 <- Sys.time()
  if ((substr(saveDir, nchar(saveDir), nchar(saveDir))) != "/") saveDir =paste0(saveDir,"/")
  if(!dir.exists(saveDir)) dir.create(saveDir)

  bins <- seq(startYr, lubridate::year(Sys.Date()), by = binYears)  #marfis really starts in 2002
  bins <- c(bins, max(bins)+binYears)
  for (f in 1:length(fleet)){
    cat("Working on", fleet[f],"\n")
    for (y in 1:(length(bins)-1)){
      cat("\t",bins[y],"\n")
      startTime2 <- Sys.time()
      loopy <- new.env()
      theName <- this <- thisTrips <- thisSumm <- thisSumm_sps <- NA
      # order is the same as Heath's document
      if (fleet[f] == "HADDOCK_FIX_5Z"){
        theName <- "HADDOCK_FIX_5Z"
        this <- fleet_haddock(type = "FIXED",area = "5ZJM", dateStart = bins[y], dateEnd =bins[y+1], useLocal =T, data.dir = data.dir, quietly = T)
      }
      if (fleet[f] == "POLLOCK_FIX_WEST"){
        theName <- "POLLOCK_FIX_WEST"
        this <- fleet_pollock(type = "FIXED",component = "WESTERN", mesh = "ALL",  dateStart = bins[y], dateEnd =bins[y+1], useLocal =T, data.dir = data.dir, quietly = T)
      }
      if (fleet[f] == "HALIBUT_45UND"){
        theName <- "HALIBUT_45UND"
        this <- fleet_halibut(vessLen = c(0,45), dateStart = bins[y], dateEnd =bins[y+1], useLocal =T, data.dir = data.dir, quietly = T)
      }
      if (fleet[f] == "HALIBUT_45PLUS"){
        theName <- "HALIBUT_45PLUS"
        this <- fleet_halibut(vessLen = c(45,999), dateStart = bins[y], dateEnd =bins[y+1], useLocal =T, data.dir = data.dir, quietly = T)
      }
      if (fleet[f] == "HALIBUT"){
        theName <- "HALIBUT"
        this <- fleet_halibut(vessLen = c(0,999), dateStart = bins[y], dateEnd =bins[y+1], useLocal =T, data.dir = data.dir, quietly = T)
      }
      if (fleet[f] == "REDFISH_2"){
        theName <- "REDFISH_2"
        this <- fleet_redfish(unit = 2, dateStart = bins[y], dateEnd =bins[y+1], useLocal =T, data.dir = data.dir, quietly = T)
      }
      if (fleet[f] == "REDFISH_3"){
        theName <- "REDFISH_3"
        this <- fleet_halibut(unit = 3, dateStart = bins[y], dateEnd =bins[y+1], useLocal =T, data.dir = data.dir, quietly = T)
      }
      if (fleet[f] == "HADDOCK_MOB_5Z"){
        theName <- "HADDOCK_MOB_5Z"
        this <- fleet_haddock(type = "MOBILE",area = "5ZJM", dateStart = bins[y], dateEnd =bins[y+1], useLocal =T, data.dir = data.dir, quietly = T)
      }
      if (fleet[f] == "SILVERHAKE"){
        theName <- "SILVERHAKE"
        this <- fleet_silverhake(dateStart = bins[y], dateEnd =bins[y+1], useLocal =T, data.dir = data.dir, quietly = T)
      }
      if (fleet[f] == "WFLOUNDER"){
        theName <- "WFLOUNDER"
        this <- fleet_winterflounder(dateStart = bins[y], dateEnd =bins[y+1], useLocal =T, data.dir = data.dir, quietly = T)
      }
      if (fleet[f] == "HADDOCK_MOB_4X5Y"){
        theName <- "HADDOCK_MOB_4X5Y"
        this <- fleet_haddock(type = "MOBILE",area = "4X5Y", dateStart = bins[y], dateEnd =bins[y+1], useLocal =T, data.dir = data.dir, quietly = T)
      }
      if (fleet[f] == "POLLOCK_MOB_WEST"){
        theName <- "POLLOCK_MOB_WEST"
        this <- fleet_pollock(type = "MOBILE", component = "WESTERN", mesh = "ALL", dateStart = bins[y], dateEnd =bins[y+1], useLocal =T, data.dir = data.dir, quietly = T)
      }
      if (fleet[f] == "POLLOCK_MOB_EAST"){
        theName <- "POLLOCK_MOB_EAST"
        this <- fleet_pollock(type = "MOBILE", component = "EASTERN", mesh = "ALL", dateStart = bins[y], dateEnd =bins[y+1], useLocal =T, data.dir = data.dir, quietly = T)
      }
      # if (fleet[f] == "SWORDFISH"){
      #   theName <- "SWORDFISH"
      #   this <- sp_swordfish(dateStart = bins[y], dateEnd =bins[y+1], useLocal =T, data.dir = data.dir, quietly = T)
      # }

      # cov <- calc_coverage(get_isdb = this$isdb, get_marfis = this$marf, quietly = T)
      thisTrips <- this$isdb$ALL_ISDB_TRIPS[!is.na(this$isdb$ALL_ISDB_TRIPS$TRIP_ID_MARF),"TRIP_ID_ISDB"]
      get_data("isdb", data.dir = data.dir, quiet = TRUE, env= loopy)
      loopy$ISTRIPS = loopy$ISTRIPS[loopy$ISTRIPS$TRIP_ID %in% thisTrips,]
      self_filter(quiet = TRUE, env= loopy)
      thisSumm <- summarize_catches('isdb', env= loopy)
      thisSumm <- thisSumm[,c("FISHSET_ID",  "MARFIS_LICENSE_NO", "CFV",  "LATITUDE", "LONGITUDE", "SPECCD_ID", "EST_NUM_CAUGHT", "EST_KEPT_WT", "EST_DISCARD_WT")]
      thisSumm_sps <- assess_privacy(df = thisSumm, agg.fields = c("EST_NUM_CAUGHT", "EST_KEPT_WT", "EST_DISCARD_WT"),
                                                sens.fields = c("MARFIS_LICENSE_NO", "CFV"),
                                                calculate = "SUM",
                                                facet.field = "SPECCD_ID",
                                                key.fields = "FISHSET_ID",
                                                lat.field = "LATITUDE",
                                                lon.field = "LONGITUDE",
                                                for.public = TRUE,
                                                create.shps = FALSE,
                                                grid.shape = "hex",
                                                file.id = NULL
                                    )
      saveRDS(this, file = paste0(saveDir,theName,"_",bins[y],".rds"))
      saveRDS(this$bycatch, file = paste0(saveDir,theName,"_bycatch_",bins[y],".rds"))
      # saveRDS(cov$summary, file = paste0(saveDir,theName,"_coverage_",bins[y],".rds"))
      saveRDS(thisSumm, file = paste0(saveDir,theName,"_raw_",bins[y],".rds"))
      saveRDS(thisSumm_sps$Grid2Min, file = paste0(saveDir,theName,"_grid_",bins[y],".rds"))
      saveRDS(thisSumm_sps$POLY_AGG, file = paste0(saveDir,theName,"_NAFO_",bins[y],".rds"))
      rm(loopy)
      cat("\t","Completed in",round( difftime(Sys.time(),startTime2,units = "secs"),0),"s\n")
      startTime2 <- NULL
    }
  }
  cat("\t","Completed in",round( difftime(Sys.time(),startTime1,units = "mins"),0),"mins\n")
  startTime2 <- NULL
}
makeFleetDataProds(startYr = 2017, binYears = 5, fleet= c('HADDOCK_FIX_5Z','POLLOCK_FIX_WEST','HALIBUT_45UND',
                                                          'HALIBUT_45PLUS','HALIBUT','REDFISH_2','REDFISH_3',
                                                          'HADDOCK_MOB_5Z','SILVERHAKE','WFLOUNDER','HADDOCK_MOB_4X5Y',
                                                          'POLLOCK_MOB_WEST','POLLOCK_MOB_EAST'),
                   saveDir = "C:/Users/McMahonM/Documents/Assistance/Bycatch/Loop202011_10")
