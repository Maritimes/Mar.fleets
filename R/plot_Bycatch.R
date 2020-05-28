plot_Bycatch <- function(obsSpp = NULL,  df=NULL, showXSpp = NULL, title = NULL, subtitle = NULL){
  library(ggplot2)
  require(scales)
  library(randomcoloR)
  ts = format(Sys.time(), "%Y%m%d_%H%M")
  fn = paste0(obsSpp)
  labCol = "COMMON"
  plotCol= "EST_NUM_CAUGHT"

  # Make a stacked column of kept/discarded -----------------------------------------------------
  dfLong <- df

  # dfLong <- dftmp
  dfLong<- dfLong[,c("SPEC", "COMMON","EST_KEPT_WT", "EST_DISCARD_WT")]
  dfLong$ALL_WT <- dfLong$EST_KEPT_WT+dfLong$EST_DISCARD_WT
  dfLong <- dfLong[with(dfLong, order(-ALL_WT, -EST_KEPT_WT)), ]
  dir_Spp_row_dfLong <- dfLong[dfLong$SPEC ==obsSpp,]
  dfLong <- dfLong[dfLong$SPEC !=obsSpp,]
  if (!is.null(showXSpp))dfLong<-head(dfLong,showXSpp-1)
  dfLong <- rbind(dir_Spp_row_dfLong, dfLong)
  dfLong$ORD <- seq(1:nrow(dfLong))
  dfLong$ALL_WT <- NULL
  dfLong <- reshape2::melt(dfLong, id.vars = c("SPEC","ORD","COMMON"))
  colnames(dfLong)[colnames(dfLong)=="variable"] <- "CATCH_TYPE"
  colnames(dfLong)[colnames(dfLong)=="value"] <- "WT"
  dfLong <- dfLong[ order(dfLong$CATCH_TYPE, decreasing=T), ]
  dfLong$CATCH_TYPE<-as.character(dfLong$CATCH_TYPE)

  s<- ggplot(data=dfLong, aes(x = reorder(COMMON, ORD), y = WT, fill = CATCH_TYPE))
  s <- s + geom_bar(stat = "identity")
  s <- s + geom_text(data=subset(dfLong, WT !=0), aes(label=WT), colour="black", size=3.5, angle=90, position = position_stack(vjust = 0.5)) #position = position_stack(vjust = 0.5),
  s <- s + labs(y="Weight (T)", x = NULL, fill = "CATCH TYPE",title = title, subtitle = subtitle)
  s <- s + theme_minimal()
  s <- s + theme(axis.text.x = element_text(size=7.5, angle = 90, hjust=1))
    s <- s + theme(title = element_text(hjust=0.5))
  s

  ggsave(plot = s, filename = paste0(fn,"_stacked_",ts,".png"), width = 14, height=8.5)
  gc()
  cat("\n", "Plot written to ", paste0(getwd(),"/", paste0(fn,"_stacked_",ts,".png")))

  # sort the data, ensure that the specified spp is first
  df <- df[with(df, order(-EST_NUM_CAUGHT, EST_KEPT_WT,EST_DISCARD_WT)), ]
  dir_Spp_row <- df[df$SPEC ==obsSpp,]
  df <- df[df$SPEC !=obsSpp,]
  if (!is.null(showXSpp))df<-head(df,showXSpp-1)
  df <- rbind(dir_Spp_row, df)
  df$ORD <- seq(1:nrow(df))

  df = df[df[,plotCol]>0,]
  df[,labCol] <- factor(df[,labCol], levels = df[,labCol])
  # Plot the total number caught ----------------------------------------------------------------
  p <- ggplot(df, aes(x= get(labCol), y=get(plotCol), fill = get(labCol)))
  p<-p + scale_y_continuous(trans = 'log10')
  p<-p + annotation_logticks(sides="l")
  p<-p + geom_bar(stat="identity")
  p<-p + labs(y="EST_NUM_CAUGHT (log)", x = NULL, fill = "COMMON", title = title, subtitle = subtitle)
  p<-p + geom_text(aes(label=get(plotCol)), position = position_stack(vjust = 0.5), colour="black", size=4, angle=90)
  p<-p + scale_fill_manual(values =c("#FF0000", distinctColorPalette(nrow(df))))
  p<-p + theme_minimal()
  p<-p + theme(axis.text.x = element_text(size=7.5, angle = 90, hjust=1))
  p
  ggsave(plot = p, filename = paste0(fn,"_tot_",ts,".png"), width = 14, height=8.5)
  gc()

  cat("\n", "Plot written to ", paste0(getwd(),"/", paste0(fn,"_tot",ts,".png")))
  cat("\n")
}
