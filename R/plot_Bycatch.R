plot_Bycatch <- function(obsSpp = NULL,  df=NULL, showXSpp = NULL, title = NULL, subtitle = NULL){
  COMMON <- ORD <- WT <- CATCH_TYPE <- NA
  ts = format(Sys.time(), "%Y%m%d_%H%M")
  fn = paste0(obsSpp)

  # Make a stacked column of kept/discarded -----------------------------------------------------
  dfLong <- df

  # dfLong <- dftmp
  dfLong<- dfLong[,c("SPEC", "COMMON","EST_KEPT_WT", "EST_DISCARD_WT")]
  dfLong$ALL_WT <- dfLong$EST_KEPT_WT+dfLong$EST_DISCARD_WT
  dfLong <- dfLong[with(dfLong, order(-ALL_WT, -EST_KEPT_WT)), ]
  dir_Spp_row_dfLong <- dfLong[dfLong$SPEC ==obsSpp,]
  dfLong <- dfLong[dfLong$SPEC !=obsSpp,]
  if (!is.null(showXSpp))dfLong<-utils::head(dfLong,showXSpp-1)
  dfLong <- rbind(dir_Spp_row_dfLong, dfLong)
  dfLong$ORD <- seq(1:nrow(dfLong))
  dfLong$ALL_WT <- NULL
  dfLong <- reshape2::melt(dfLong, id.vars = c("SPEC","ORD","COMMON"))
  colnames(dfLong)[colnames(dfLong)=="variable"] <- "CATCH_TYPE"
  colnames(dfLong)[colnames(dfLong)=="value"] <- "WT"
  dfLong <- dfLong[ order(dfLong$CATCH_TYPE, decreasing=T), ]
  dfLong$CATCH_TYPE<-as.character(dfLong$CATCH_TYPE)

  s<- ggplot2::ggplot(data=dfLong, ggplot2::aes(x = stats::reorder(COMMON, ORD), y = WT, fill = CATCH_TYPE))
  s <- s + ggplot2::geom_bar(stat = "identity")
  s <- s + ggplot2::geom_text(data=subset(dfLong, WT !=0), ggplot2::aes(label=WT), colour="black", size=3.5, angle=90, position =  ggplot2::position_stack(vjust = 0.5)) #position = position_stack(vjust = 0.5),
  s <- s + ggplot2::labs(y="Weight (kgs)", x = NULL, fill = "CATCH TYPE",title = title, subtitle = subtitle)
  s <- s + ggplot2::theme_minimal()
  s <- s + ggplot2::theme(axis.text.x =  ggplot2::element_text(size=7.5, angle = 90, hjust=1))
  s <- s + ggplot2::theme(title = ggplot2::element_text(hjust=0.5))
  s

  ggplot2::ggsave(plot = s, filename = paste0(fn,"_stacked_",ts,".png"), width = 14, height=8.5)
  gc()
  cat("\n", "Plot written to ", paste0(getwd(),"/", paste0(fn,"_stacked_",ts,".png")))

  # sort the data, ensure that the specified spp is first
  df <- df[with(df, order(-EST_NUM_CAUGHT, EST_KEPT_WT,EST_DISCARD_WT)), ]
  dir_Spp_row <- df[df$SPEC ==obsSpp,]
  df <- df[df$SPEC !=obsSpp,]
  if (!is.null(showXSpp))df<-utils::head(df,showXSpp-1)
  df <- rbind(dir_Spp_row, df)
  df$ORD <- seq(1:nrow(df))
  df = df[df$EST_NUM_CAUGHT>0,]
  if (nrow(df)>0){
    df$COMMON <- factor(df$COMMON, levels = df$COMMON)
  # Plot the total number caught ----------------------------------------------------------------
  p <- ggplot2::ggplot(df, ggplot2::aes(x= "COMMON", y="EST_NUM_CAUGHT", fill = "COMMON"))
  p<-p + ggplot2::scale_y_continuous(trans = 'log10')
  p<-p + ggplot2::annotation_logticks(sides="l")
  p<-p + ggplot2::geom_bar(stat="identity")
  p<-p + ggplot2::labs(y="EST_NUM_CAUGHT (log)", x = NULL, fill = "COMMON", title = title, subtitle = subtitle)
  p<-p + ggplot2::geom_text(ggplot2::aes(label="EST_NUM_CAUGHT"), position = ggplot2::position_stack(vjust = 0.5), colour="black", size=4, angle=90)
  p<-p + ggplot2::scale_fill_manual(values =c("#FF0000", randomcoloR::distinctColorPalette(nrow(df))))
  p<-p + ggplot2::theme_minimal()
  p<-p + ggplot2::theme(axis.text.x = ggplot2::element_text(size=7.5, angle = 90, hjust=1))
  p
  ggplot2::ggsave(plot = p, filename = paste0(fn,"_tot_",ts,".png"), width = 14, height=8.5)
  gc()

  cat("\n", "Plot written to ", paste0(getwd(),"/", paste0(fn,"_tot",ts,".png")))
  cat("\n")
  }
}
