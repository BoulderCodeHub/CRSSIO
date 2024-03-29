% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sac_year_type_calc.R
\name{sac_year_type_calc}
\alias{sac_year_type_calc}
\title{Compute Sacramento Valley Water Year Index for CRSS}
\usage{
sac_year_type_calc(wy_vol = NULL, co_int_nf = NULL)
}
\arguments{
\item{wy_vol}{xts object containing Sacramento 4-river index water year
volumes in million acre-feet. xts object must have yearly periodicity.}

\item{co_int_nf}{\link{nfd} object that has annual, intervening data and has the
"wy" year type. It also must have 29 sites that are named the same as the
expected site names in a \link{crss_nf} object.}
}
\value{
xts object. When using \code{wy_vol} will have the same number of columns
as \code{wy_vol}. When using \code{co_int_nf} will have the same number of columns as
there are traces in \code{co_int_nf}.
}
\description{
\code{sac_year_type_calc()} calculates the Sacramento Valley Water Year (WY) Index
using either the Sacramento 4 river index water year volume, or Colorado
River WY intervening natural flow data. \strong{Note that these methods are
implemented only for purposes of generating an index to use in CRSS.}
}
\details{
CRSS uses the Sacramento Valley Water Year Index to determine MWD's desired
annual ICS activity. This method adds plausible variability to the CRSS
projections based on MWD’s needs for storage or delivery of Colorado River
water based on the availability of their other California supplies. The index
is available for 1906-present (see \code{\link[=sac_year_type_get]{sac_year_type_get()}}), which is used for
hydrology scenarios that rely on historical data. For other scenarios, e.g.,
paleo, or statistical generated, the index must be calculated from other
available data.

If the Sacramento 4 river index water year (WY) volume is available (like it
is going back to 900 C.E. based on paleo reconstructions), the WY volume can
be used to recreate the index values. In this case, the following formula is
used, which is derived from the official formula documented at
\url{http://cdec.water.ca.gov/cgi-progs/iodir/WSIHIST}.

\deqn{index_t = 0.323 * RWY + 0.3 * min(index_{t-1}, 10.0)}

where \code{RWY} is the Sacramento 4 river WY volume \strong{in million acre-feet}.
Note that because the index
in the first year depends on the index value from the previous year, which is
unknown, the 0th index value is assumed to be an average year with an index
value of 7.8.

When the Sacramento 4 river index WY volume is unknown, the index can be
generated from the Colorado River natural flow data since these must always
be available for use in CRSS. The index is created based on a decision tree
that is fit to the water year intervening natural flow for all 29 sites in
the Colorado River Basin. See XXXX for details on the development and tuning
of this decision tree.
}
