% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fleet_redfish.R
\name{fleet_redfish}
\alias{fleet_redfish}
\title{fleet_redfish}
\usage{
fleet_redfish(
  marfGear = c(12, 14, 15, 21, 22, 41),
  unit = NULL,
  useLocal = NULL,
  socks = FALSE,
  ...
)
}
\arguments{
\item{marfGear}{default is \code{c(12,14,15,21,22,41)}. This is a vector of MARFIS gear codes
known to have been used by this fleet. The default values can be replaced with a subset of these to
only return a gear-specific subset of the fleet's fishing activity.  If other values are provided,
the script will not run.}

\item{unit}{default is \code{NULL}.  Valid selections include \code{"UNIT2"} and \code{"UNIT3"}}

\item{useLocal}{default is \code{FALSE}. This specifies whether to run the script against local
data or against Oracle (requires network or VPN).
Depending on your value for \code{useLocal}, different values become necessary.
\itemize{
 \item{useLocal=TRUE} This implies that you have local data you want to use.
 \item{useLocal=FALSE} This implies that you have will query Oracle for the necessary data.
 Ensure you include:
  \itemize{
      \item param \code{cxn} a valid oracle connection (e.g. an roracle, rodbc or dbi connection object)
    }
}}

\item{socks}{default is \code{FALSE}. Normally, un-QC'd wrappers generate a prompt forcing the
user to acknowledge that they are aware that the script needs further testing.  Setting this to
TRUE will prevent the prompt from showing up. Why 'socks'?  You had to be there.}

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
    \item{\code{manualMatch}}{default is \code{FALSE}. This parameter is only used when calling functions
from \code{manual_matcher()}.  It ensures that the functions work properly with its reduced
input format.}
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
\value{
specific returned objects can be specified by the user, but the default result is a list
of objects.  The list includes marfis data, isdb data, information related to the matching, and a
breakdown of where the various trips and sets occurred, specifically:
\itemize{
 \item \code{params} -  this is a list containing information about the extraction
 \itemize{
 \item \code{user} - this contain all of the parameters sent to the function (including defaults, user-provided and hardcoded)
 \item \code{fleet} - this is a list object containing 3 dataframes that contain the information used to identify the fleet.
 These will include licencesCore, licecesAreas, and licencesGearSpecs.  Depending on how the fleet is defined, one or more of
 these may be empty.
 }
  \item \code{fleet} - This is a dataframe of the unique combinations of (MARFIS) LICENCE_ID, VR_NUMBER and GEAR_CODE that
  was found for this fleet during the specified period
  \item \code{FLEET_ACTIVITY} - This is a dataframe of identifiers for all of the (MARFIS) fishing activity undertaken
  by vessels of this fleet during the specified period (i.e. LICENCE_ID, PRO_SPC_INFO_ID, LOG_EFRT_STD_INFO_ID, GEAR_CODE,
  MON_DOC_ID, VR_NUMBER, and several dates associated with the trip)
  \item \code{marf} - This is a list of 3 sets of information for the commercial catch data (i.e. marfis):
  \itemize{
  \item \code{MARF_TRIPS}
  \item \code{MARF_SETS}
  \item \code{MARF_MATCH} This is a special dataframe containing information that can be used to link
  the commercial data to the ISDB data
  }
  \item \code{isdb} - This is a list of data objects from the ISDB db:
  \itemize{
  \item \code{ISDB_TRIPS} These are ISDB trips that are associated with MARFIS trips from the \code{marf$MARF_TRIPS} object above
  \item \code{ISDB_SETS} These are all of the ISDB sets associated with the ISDB_TRIPS (matched and unmatched)
  \item \code{ISDB_CATCHES} This is the data associated with the records in ISDB_TRIPS
  \itemize{
  \item \code{ALL} This is the raw data from ISCATCHES for the trips found in ISDB_TRIPS
  \item \code{SUMMARY} This is the data from ISCATCHES for all of the trips found in ISDB_TRIPS, summarized by species. Each species
  has calculated aggregate values for "EST_NUM_CAUGHT", EST_KEPT_WT", "EST_DISCARD_WT" and "EST_COMBINED_WT"
  }
  }
  \item \code{matches} This is a list item that contains all of the information used to assigne matches between MARFIS and ISDB
  \itemize{
  \item \code{MATCH_SUMMARY_TRIPS} This is a simple breakdown of the various approaches used for matching, and the relative success of each.
  Matches can occur using multiple approaches, so these can not be added up.  This list also includes "\code{Likely_Swapped_VR_Lic}" which
  indicates how may matches seem to have the values for LICENCE_ID and VR_NUMBER reversed, and includes the count of how many rows are present
  in both \code{Multimatches} and \code{Umatchables}
  \item \code{MATCH_DETAILS} This is a dataframe of all of the MARFIS and ISDB trips that have been associated with each other, and whether
  or not they were matched on each of the possible approaches
  \item \code{ISDB_UNMATCHABLES} These are the trips from MARFIS that included ISDB-type information (e.g. Observer ID, ISDB Trip name, etc), but
  for which no ISDB match could be found.
  \item \code{ISDB_MULTIMATCHES} These are ISDB trips that were found to be match multiple MARFIS trips equally well.
  }
  \item \code{location_summary} - This is a list of 1 or more dataframes that breaks down the various trips
  and sets by the areas in which they occurred.  NAFO locations are reported for MARFIS trips, MARFIS sets and ISDB sets
  (not ISDB trips).  These reported locations are shown, as are the "calculated" locations, which are based on the
  reported latitudes and longitudes. No "calculated" locations are shown for MARFIS trips, as there are no coordinates for
  the trip level.  If a custom value for \code{areaFile} was sent (i.e. not "NAFOSubunits_sf"), a second dataframe breaking
  down the sets by the custom area will also be provided.
  }
}
\description{
This is a wrapper function that facilitates extracting information for the redfish fleet.
All of the information used to identify fleets is stored in the package's associated data files -
LIC_CORE, LIC_AREAS, and LIC_GEAR_SPEC.  The various wrappers can have different options (e.g.
MOBILE vs FIXED, WESTERN vs EASTERN, 4XY vs 5ZJM, small mesh vs large mesh, diamond vs square
mesh, etc), and depending on which options are selected, different fleets are identified, and
their data is extracted.
}
\details{
Licence Information for any fleet is accessible via the following calls.  Please replace
 "<fleet>", with this fleet's actual fleet value identified in the NOTE, below:
\itemize{
  \item \code{Licence Type, Subtype, Gear and Species Information (if applicable)}
  \code{Mar.fleets::LIC_CORE[Mar.fleets::LIC_CORE$FLEET=="<fleet>",]}
  \item \code{Licence Areas (if applicable)} \code{Mar.fleets::LIC_AREAS[Mar.fleets::LIC_AREAS$FLEET=="<fleet>",]}

  If different areas/components/units are available for this fleet, the areas associated with
  each can be differentiated by the differing values of \code{FLEET_AREA_ID }. For example, the
  Redfish fleet is divided into Units 2 and 3.  All of the NAFO areas associated with either of
  these units these can be found in via \code{Mar.fleets::LIC_AREAS[Mar.fleets::LIC_AREAS$FLEET=="REDFISH",]},
  but the NAFO areas associated with the Unit 2 fleet are those with \code{FLEET_AREA_ID} == \code{UNIT2}.
  \item \code{Licence Gear Specifications (if applicable)} \code{Mar.fleets::LIC_GEAR_SPEC[Mar.fleets::LIC_GEAR_SPEC$FLEET=="<fleet>",]}

   If particular gear size/types are allowed, the range of sizes for each are specified by the
   MIN and MAX fields.  If aspects of the fleet are defined by the gear size, multiple records
   may be present.  For example, the SMALL mesh fleet will have different max and min values than
   the LARGE MESH fleet.  These records can correspond with fleet areas, but do not have to.  In
   this case, the gear associated with catching redfish in UNIT 2 is different than what's
   allowed in UNIT 3, so the LIC_GEAR_SPEC table differentiates the gear by having different
   entries in \code{FLEET_GEARSPECS_ID} (i.e. \code{UNIT2} vs \code{UNIT3}).  The mobile POLLOCK
   fleet also has multiple categories of gear sizes, but they are not related to different areas
   - the entries in \code{FLEET_GEARSPECS_ID} are just \code{SMALL} and \code{LARGE}.  Differing
   values of Type have not been implemented, but the field exist such that gear can be filtered
   by Diamond vs Square mesh.
}
}
\note{
Hardcoded parameters for this fleet are as follows:
\itemize{
  \item \code{marfSpp} = 120
  \item \code{isdbSpp} = 23
  \item \code{tripcd_id} = 23
  \item \code{fleet} = "REDFISH"
}
The following parameters are "softcoded" - any or all of the values can be
provided, but other values are not allowed.
\itemize{
  \item \code{marfGear} = c(12,14,15,21,22,41)
}
}
\examples{
\dontrun{
db <- fleet_redfish(unit = "UNIT2",
                    useLocal = F,
                    year = 2018,
                    cxn  = <valid Oracle Connection>
                    )
local <- fleet_redfish(unit = "UNIT2",
                       useLocal = T,
                       year = 2018
                      )
                       }
}
\seealso{
Other fleets: 
\code{\link{fishin_CHPs}()},
\code{\link{fleet_bluefin}()},
\code{\link{fleet_cucumber}()},
\code{\link{fleet_flatfish}()},
\code{\link{fleet_gaspereau}()},
\code{\link{fleet_hagfish}()},
\code{\link{fleet_halibut}()},
\code{\link{fleet_herring}()},
\code{\link{fleet_jonah}()},
\code{\link{fleet_redCrab}()},
\code{\link{fleet_rockCrab}()},
\code{\link{fleet_scallop}()},
\code{\link{fleet_sculpin}()},
\code{\link{fleet_shrimp}()},
\code{\link{fleet_silverhake}()},
\code{\link{fleet_snowcrab}()},
\code{\link{fleet_surfclam}()},
\code{\link{fleet_swordfishTunaShark}()},
\code{\link{fleet_urchin}()},
\code{\link{fleet_whelk}()}
}
\author{
Mike McMahon, \email{Mike.McMahon@dfo-mpo.gc.ca}
}
\concept{fleets}
