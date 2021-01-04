#' @title fleet_practice
#' @description This function is a wrapper function that facilitates extracting the following
#' information for the practice fleets:
#' \itemize{
#'   \item \code{fleet} - This is a dataframe of identifiers for all of the various trips undertaken by the
#'   selected fleet for the specified period (e.g. VRNs, licence IDs, Monitoring Document #s, etc)
#'   \item \code{marf} - This is a list of 3 sets of information for the commercial catch data (i.e. marfis)-
#'   the trips, the sets, and a special dataframe containing information that can be used to link
#'   the commercial data to the ISDB data
#'   \item \code{isdb} - This is a list of 4 data objects - 2 of which are all of the discovered ISDB data
#'   TRIPS and SETS for the fleet, as well as the TRIPS and SETS from the observer data that were
#'   sucessfully matched with the MARFIS data
#'   \item \code{bycatch} - This is a dataframe with the various species that were observed during observed
#'   trips.  For each species, the estimated number caught, the estimated kept wt (kgs) and the
#'   estimated discarded wt(kg) are all captured
#'   }
#' @param vessLen default is \code{NULL}.  This is a vector of vessel lengths.  If it is not NULL or
#' "all", it will be used to restrict vessels by their size.  The supplied vector will only be assessed
#' for its max and min values, so if you wanted vessels up to and including 45ft, you could enter either
#' of the following - \code{c(0,45)} or \code{seq(0,45,1)}.
#' @param ... other arguments passed to methods
#' @examples \dontrun{
#' stuff <- fleet_practice(data.dir = "C:/myData")
#' }
#' @family fleets
#' @return list of objects, including marfis data, isdb data, information for matching isdb
#' and marfis data, and a summary of bycatch
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfSpp} = 130
#'   \item \code{nafoCode} = c('3N\%','3O\%','3PS\%','4V\%','4W\%','4X\%','5\%')
#'   \item \code{gearCode} = c(50,51)
#'   \item \code{mdCode} = c(1, 29)
#' }
#' @export
fleet_practice <- function(...){

  lics = data.frame("types" =     c(0, 1,10,11),
                    "subtypes" = c(28,24,14,24))
  # lics <- data.frame(types=integer(),
  #                  subtypes=integer(),
  #                  stringsAsFactors=FALSE)
  # lics <- rbind(lics, c(0,28))
  #
  # , c(1,24),c(10,14),c(11,24))

  # MARBYCATCH_LIC <- MARBYCATCH_LIC[which(((MARBYCATCH_LIC$LICENCE_TYPE_ID %in% c(0) & MARBYCATCH_LIC$LICENCE_SUBTYPE_ID %in% c(28)) |
  #                                           (MARBYCATCH_LIC$LICENCE_TYPE_ID %in% c(1) & MARBYCATCH_LIC$LICENCE_SUBTYPE_ID %in% c(24)) |
  #                                           (MARBYCATCH_LIC$LICENCE_TYPE_ID %in% c(10) & MARBYCATCH_LIC$LICENCE_SUBTYPE_ID %in% c(14)) |
  #                                           (MARBYCATCH_LIC$LICENCE_TYPE_ID %in% c(11) & MARBYCATCH_LIC$LICENCE_SUBTYPE_ID %in% c(24))) &
  #                                          MARBYCATCH_LIC$GEAR_TYPE_ID==1),]
  marfSpp=130
  nafoCode=c('3N%','3O%','3PS%','4V%','4W%','4X%','5%')
  # gearCode = c(50,51)
  # mdCode = c(1, 29)

  argsFn <- as.list(environment())
  argsUser <- list(...)

  if((length(argsUser$debug)>0) && (argsUser$debug == TRUE)) {
    Mar.utils::where_now(inf = as.character(sys.calls()[[sys.nframe()]]))
    startTime=Sys.time()
  }

  data <- do.call(get_all, list(argsFn= argsFn, argsUser=argsUser))
  if(exists("startTime")) cat("\n","Completed in",round( difftime(Sys.time(),startTime,units = "secs"),0),"secs\n")
  return(data)
}
