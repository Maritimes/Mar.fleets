% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quick_map.R
\name{quick_map}
\alias{quick_map}
\title{quick_map}
\usage{
quick_map(
  data = NULL,
  plotMARF = TRUE,
  clusterMARF = TRUE,
  overloadMARF = "cluster",
  plotMARFSurf = FALSE,
  marfSpp = NULL,
  plotISDB = TRUE,
  clusterISDB = TRUE,
  overloadISDB = "cluster",
  plotISDBSurf = FALSE,
  isdbField = "EST_COMBINED_WT",
  isdbSpp = NULL,
  title = NULL,
  vms = NULL,
  bathy = TRUE,
  surfRes = "low"
)
}
\arguments{
\item{data}{default is \code{NULL}. This is the entire output from any of the fleet wrappers.}

\item{plotMARF}{default is \code{TRUE}. Should MARFIS data be plotted? If there are more than
1500 positions, the data will be displayed clustered.}

\item{clusterMARF}{default is \code{TRUE}. If \code{TRUE}, MARF data will be grouped until
the map is zoomed in sufficiently  If \code{FALSE}, every MARF data point will be shown. If the
MARFIS data has > 1500 positions, it will be clustered regardless of this setting.}

\item{overloadMARF}{default is \code{"cluster"}. Valid values are "cluster", and "random". This
map gets really slow when dealing with many positions, and this parameter indicates what should
be done with the MARFIS data if there are more sets than can reasonably be shown (i.e >1500).
\code{"cluster"} causes the data to be shown as grouped symbols which expand into discrete points
as you zoom in.  \code{"random"} just grabs a random selection of 1500 points, and plots those.}

\item{plotMARFSurf}{default is \code{FALSE}. If \code{TRUE}, an interpolated surface will be
generated for the MARFIS data. MARFIS point data will be interpolated using the "RND_WGT_KGS"
field.}

\item{marfSpp}{default is \code{NULL}.  If nothing is provided, the default directed species
will be pulled from the input data (e.g. if the data from fleet_halibut() is provided, halibut
(i.e. "130" will be used.))  Any  marfis species code(s) found in <data>$marf$MARF_CATCHES can be
used.}

\item{plotISDB}{default is \code{TRUE}. Should ISDB data be plotted? If there are more than
1500 positions, the data will be displayed clustered.}

\item{clusterISDB}{default is \code{TRUE}. If \code{TRUE}, ISDB data will be grouped until
the map is zoomed in sufficiently  If \code{FALSE}, every ISDB data point will be shown.  If the
ISDB data has > 1500 positions, it will be clustered regardless of this setting.}

\item{overloadISDB}{default is \code{"cluster"}. Valid values are "cluster", and "random". This
map gets really slow when dealing with many positions, and this parameter indicates what should
be done with the ISDB data if there are more sets than can reasonably be shown (i.e >1500).
\code{"cluster"} causes the data to be shown as grouped symbols which expand into discrete points
as you zoom in.  \code{"random"} just grabs a random selection of 1500 points, and plots those.}

\item{plotISDBSurf}{default is \code{FALSE}. If \code{TRUE}, an interpolated surface will be
generated for the ISDB data. ISDB data contains several fields data for many species.  By default,
the interpolation will use the "EST_COMBINED_WT" field for the default directed species, but these
options can be overwritten by changing the values of \code{isdbField} and \code{isdbSpp},
respectively.}

\item{isdbField}{default is \code{"EST_COMBINED_WT"}. Other valid values are "EST_NUM_CAUGHT",
"EST_KEPT_WT", and "EST_DISCARD_WT".}

\item{isdbSpp}{default is \code{NULL}.  If nothing is provided, the default directed species
will be pulled from the input data (e.g. if the data from fleet_halibut() is provided, halibut
(i.e. "30" will be used.)).  Any isdb species code(s) found in <data>$isdb$ISDB_CATCHES$ALL can
be used.  Entering \code{"?"} generates a user-selectable list of all of the available species.}

\item{title}{default is \code{NULL}. This will be shown as the title of the map.}

\item{vms}{default is \code{NULL}. This is optional, but can be the output from \code{get_vmstracks()}.
If provided, VMS data will be plotted.}

\item{bathy}{default is \code{TRUE}. If \code{TRUE}, a bathymetry layer will be available.}

\item{surfRes}{default is \code{"low"}. This determines the resolution of any output surfaces.  Valid
values are "low", "med" or "high".  Higher values increase the time it takes to generate the surface.}
}
\value{
a leaflet map.
}
\description{
This function generates a simple leaflet plot for the output from Mar.bycatch
functions.
}
\examples{
\dontrun{
redfishresults <- fleet_redfish(unit=3, year = "2017", useLocal=T,data.dir="c:/data/")
redfishVMS<-get_vmstracks(data = redfishresults,
                          cxn = <valid oracle connection>)
quick_map(data=redfishresults, vms= redfishVMS)
       }
}
\seealso{
Other simpleproducts: 
\code{\link{get_vmstracks}()},
\code{\link{summarize_locations}()},
\code{\link{summarizer}()}
}
\author{
Mike McMahon, \email{Mike.McMahon@dfo-mpo.gc.ca}
}
\concept{simpleproducts}
