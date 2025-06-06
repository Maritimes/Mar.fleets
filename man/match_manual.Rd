% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match_manual.R
\name{match_manual}
\alias{match_manual}
\title{match_manual}
\usage{
match_manual(
  TRIP_ID_MARF = NULL,
  TRIP_ID_ISDB = NULL,
  manualMatch = T,
  extract_user = NULL,
  extract_computer = NULL,
  ...
)
}
\arguments{
\item{TRIP_ID_MARF}{The default is \code{NULL}. This can be \code{"ALL"}, or a vector of 1 or
more MARFIS TRIP_ID values you wish to attempt to find matches for in the ISDB schema. Discrete
values (i.e. not "ALL") can ONLY be provided to one of TRIP_ID_MARF OR TRIP_ID_ISDB - not both.}

\item{TRIP_ID_ISDB}{The default is \code{NULL}. This can be \code{"ALL"}, or a vector of 1 or
more ISDB TRIP_ID values you wish to attempt to find matches for in the MARFISSCI schema.
Discrete values (i.e. not "ALL") can ONLY be provided to one of TRIP_ID_MARF OR TRIP_ID_ISDB -
not both.}

\item{manualMatch}{default is \code{FALSE}. This parameter is only used when calling functions
from \code{manual_matcher()}.  It ensures that the functions work properly with its reduced
input format.}

\item{extract_user}{default is \code{NULL}.  This parameter can be used with
\code{extract_computer} to load encypted data files extracted by another user
and/or computer}

\item{extract_computer}{default is \code{NULL}.  This parameter can be used with
\code{extract_user} to load encypted data files extracted by another user
and/or computer}

\item{...}{
  Arguments passed on to \code{\link[=set_defaults]{set_defaults}}
  \describe{
    \item{\code{year}}{default is \code{NULL}.  \code{year} can be used if data should be extracted for an
entire calendar year (i.e. Jan 1 --> Dec 31).  The format is
\code{YYYY}, e.g. \code{year = 2018}.  \code{dateStart} takes priority over \code{year} if both
are provided.}
    \item{\code{dateStart}}{default is \code{NULL}.  This identifies the start date of the search window, in
the format \code{YYYY-MM-DD} (e.g. \code{dateStart = "2019-02-19"}).
If no \code{dateEnd} is provided, the window will be for 1 year (i.e, 365 days).  ,}
    \item{\code{dateEnd}}{default is \code{NULL} format is \code{YYYY-MM-DD}, e.g. \code{dateEnd = "2019-02-19"}.
\code{dateEnd} must be associated with a valid entry of
\code{dateStart} to identify a block of time for a data extraction  (e.g. Jun18, 2018 -->
August 27, 2019).}
    \item{\code{marfSpp}}{default is \code{'all'}. The marfis species code, usually sent by the fleet
wrapper.}
    \item{\code{marfGear}}{default is \code{'all'}, but all wrappers have (overwritable) fleet-specific
values.  This is a vector of MARFIS gear codes known to have caught this species.}
    \item{\code{isdbSpp}}{default is \code{'all'}. The ISDB species code, usually sent by the fleet wrapper}
    \item{\code{tripcd_id}}{default is \code{NULL}.  If a tripcd_id from ISDB is provided, all matting
records will be examined for matches}
    \item{\code{returnMARFIS}}{default is \code{TRUE}. Do you want a list object containing marfis trip and
set information as part of your results?}
    \item{\code{returnISDB}}{default is \code{TRUE}. Do you want a list object containing isdb trip and set
information as part of your results? (requires \code{returnMARFIS = T})}
    \item{\code{areaFile}}{default is \code{'NAFOSubunits_sf'}.  This is used to identify which areas to
check the trips and sets against. By default,
Mar.data::NAFOSubunits_sf is ued, but any objects in Mar.data could be used.}
    \item{\code{areaFileField}}{default is \code{'NAFO_1'}. This is a field within the \code{areas} object
which specifies exactly which field of the areas object data
should be compared against.}
    \item{\code{nafoDet}}{default is \code{2}, but values between \code{1} and \code{4} are acceptable. This
specifies the level of detail that will be used in the summarized locations table.  Using the
default value of 2, trips and sets will be summarized by areas such as "4X", "4V" and "5Z" (i.e 2
characters).  If set to "1", areas would be more general  (e.g. "3", "4", "5"; i.e. 1 character),
while a value like 4 would summarize the trips and sets into very specific NAFO subunits (e.g.
"3PSA","4VSB" and "5ZEM")}
    \item{\code{keepSurveyTrips}}{default is \code{TRUE}. Within the ISDB database are non-commercial,
survey trips.  Setting this to \code{FALSE} ensures these trips are dropped.}
    \item{\code{keepMissingGear}}{default is \code{TRUE}. Some fleets have particular allowable gear sizes
and types (see gearSpecs). Many cases exist where all of the gear details are not filled it.
When this parameter is set to \code{TRUE}, these 'unknown' types and sizes are retained, and the
values are set to -999.  If it is set to \code{FALSE}, any gears with missing values are dropped
- and they are not included in the results.}
    \item{\code{maxTripDiff_Hr}}{default is \code{48}. Any MARFIS and ISDB trips that vary by more than the
# of days specified here will NOT be considered matches (on the basis of common Vessel, licence
and date).  They may still match on confirmation codes and/or trip names.}
    \item{\code{maxSetDiff_Hr}}{default is \code{48}. Any MARFIS and ISDB sets that vary by more than the
# of hours specified here will NOT be considered matches.}
    \item{\code{maxSetDiff_Km}}{default is \code{100}. Any MARFIS and ISDB sets with positions more than the
# of kilometers specified here will NOT be considered matches.}
    \item{\code{dropUnmatchedISDB}}{default is \code{TRUE}.}
    \item{\code{socks}}{default is \code{FALSE}. Normally, un-QC'd wrappers generate a prompt forcing the
user to acknowledge that they are aware that the script needs further testing.  Setting this to
TRUE will prevent the prompt from showing up. Why 'socks'?  You had to be there.}
    \item{\code{debug}}{default is \code{FALSE}. If TRUE, this parameter causes the package to run in
debug mode, providing much extraneous information.}
    \item{\code{debugLics}}{default is \code{NULL}.  If a vector of LICENCE_IDs is provided, the script will
provide information about when the script drops them from
consideration.}
    \item{\code{debugVRs}}{default is \code{NULL}.  If a vector of VR numbers is provided, the script will
provide information about when the script drops them from
consideration.}
    \item{\code{debugMARFTripIDs}}{default is \code{NULL}.  If a vector of MARFIS trip IDs is provided, the
script will provide information about when the script drops them from
consideration.}
    \item{\code{debugISDBTripIDs}}{default is \code{NULL}.  If a vector of ISDB trip IDs is provided, the
script will provide information about when the script drops them from
consideration.}
    \item{\code{debugISDBTripNames}}{default is \code{NULL}.  If a vector of ISDB trip names is provided,
the script will provide information about when the script drops them from
consideration. Trip "names" are typically in a format like "J18-1234" or "A18-1234A".}
  }}
}
\description{
This function was created to circumvent the "fleet" aspect of the wrappers, and
match marfis data with ISDB data.
}
\author{
Mike McMahon, \email{Mike.McMahon@dfo-mpo.gc.ca}
}
