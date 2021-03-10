#' spLookups Metadata
#'
#' This is an rda file with both observer and MARFIS species codes.  It is used to allow
#' identification of ISDB species, when only marfis species codes are required
#'
#' @format  rda
#' \describe{
#' \item{SPECCD_ID}{ISDB species code}
#' \item{SCIENTIFIC}{Scientifi name}
#' \item{COMMON}{Common name}
#' \item{MARFIS_CODE}{marfis species code}
#' }
"spLookups"

#' licCore Metadata
#'
#' This is an rda file with codes for identifying participants in different fleets.  It includes the fields identified below:
#' @docType data
#' @usage data(licCore)
#' @format  rda
#' \describe{
#' \item{FLEET}{name of fleet}
#' \item{LIC_TYPE}{Licence Type ID}
#' \item{LIC_SUBTYPE}{Licence Subtype ID}
#' \item{LIC_SP}{Licence species ID}
#' \item{LIC_GEAR}{Licence gear ID}
#' }
"licCore"

#' licAreas Metadata
#'
#' This is an rda file with codes for identifying participants in different fleets.  It includes the fields identified below:
#' @docType data
#' @usage data(licAreas)
#' @format  rda
#' \describe{
#' \item{FLEET}{name of fleet}
#' \item{FLEET_AREA_ID}{name of subset of fleet for these areas}
#' \item{AREA_TYPE}{category of area}
#' \item{AREA}{specific area}
#' }
"licAreas"

#' licGearSpecs Metadata
#'
#' This is an rda file with codes for identifying participants in different fleets.  It includes the fields identified below:
#' @docType data
#' @usage data(licGearSpecs)
#' @format  rda
#' \describe{
#' \item{FLEET}{name of fleet}
#' \item{FLEET_GEARSPECS_ID}{name of subset of fleet with these gear sizes}
#' \item{MIN}{smallest size cutoff}
#' \item{MAX}{largest size cutoff}
#' \item{TYPE}{this describes the allowable gear - e.g. "S" (square); "D" (diamond)}
#' }
"licGearSpecs"
