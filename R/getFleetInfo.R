getFleetInfo <- function(fleet = NULL){
  all <- read.csv('data/fleetDefns.csv')
  this <- all[all$FLEET == fleet,]
  return(this)
}
