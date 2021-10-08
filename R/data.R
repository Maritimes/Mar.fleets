#' LIC_CORE Metadata
#'
#' This is an rda file with codes for identifying participants in different fleets.  It includes the fields identified below:
#' @docType data
#' @usage data(LIC_CORE)
#' @format  rda
#' \describe{
#' \item{FLEET}{name of fleet}
#' \item{LIC_TYPE}{Licence Type ID}
#' \item{LIC_SUBTYPE}{Licence Subtype ID}
#' \item{LIC_SP}{Licence species ID}
#' \item{LIC_GEAR}{Licence gear ID}
#' }
"LIC_CORE"

#' LIC_AREAS Metadata
#'
#' This is an rda file with codes for identifying participants in different fleets.  It includes the fields identified below:
#' @docType data
#' @usage data(LIC_AREAS)
#' @format  rda
#' \describe{
#' \item{FLEET}{name of fleet}
#' \item{FLEET_AREA_ID}{name of subset of fleet for these areas}
#' \item{AREA_TYPE}{category of area}
#' \item{AREA}{specific area}
#' }
"LIC_AREAS"

#' LIC_GEAR_SPEC Metadata
#'
#' This is an rda file with codes for identifying participants in different fleets.  It includes the fields identified below:
#' @docType data
#' @usage data(LIC_GEAR_SPEC)
#' @format  rda
#' \describe{
#' \item{FLEET}{name of fleet}
#' \item{FLEET_GEARSPECS_ID}{name of subset of fleet with these gear sizes}
#' \item{MIN}{smallest size cutoff}
#' \item{MAX}{largest size cutoff}
#' \item{TYPE}{this describes the allowable gear - e.g. "S" (square); "D" (diamond)}
#' }
"LIC_GEAR_SPEC"

#' GEARS_MARFIS Metadata
#'
#' This is an rda file of a lookup table used to associate different MARFIS gear codes with name of the gear:
#' @docType data
#' @usage data(GEARS_MARFIS)
#' @format  rda
#' \describe{
#' \item{GEAR_CODE}{code identifying the type of gear}
#' \item{GEAR}{name of the gear}
#' }
"GEARS_MARFIS"

#' SPECIES_MARFIS Metadata
#'
#' This is an rda file of a lookup table used to associate different MARFIS species codes with the name of the species:
#' @docType data
#' @usage data(SPECIES_MARFIS)
#' @format  rda
#' \describe{
#' \item{SPECIES_CODE}{code identifying the species}
#' \item{SPECIES_NAME}{name of the species/species group}
#' }
"SPECIES_MARFIS"

#' SPECIES_ISDB Metadata
#'
#' This is an rda file of a lookup table used to associate different ISDB species codes with the name of the species:
#' @docType data
#' @usage data(SPECIES_ISDB)
#' @format  rda
#' \describe{
#' \item{SPECCCD_ID}{code identifying the species}
#' \item{COMMON}{common name of the species}
#' \item{SCIENTIFIC}{scientific name of the species}
#' }
"SPECIES_ISDB"
