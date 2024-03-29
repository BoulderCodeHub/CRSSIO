% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nfd_stats.R, R/nfd_pdf.R
\name{plot.nfd_stats}
\alias{plot.nfd_stats}
\alias{plot.nfd_pdf}
\title{Plot nfd_stats and nfd_pdf Objects}
\usage{
\method{plot}{nfd_stats}(x, ref = NULL, base_units = NULL, show = TRUE, ...)

\method{plot}{nfd_pdf}(x, ref = NULL, base_units = NULL, show = TRUE, ...)
}
\arguments{
\item{x}{An object inheriting from \code{nfd_stats} or \code{nfd_pdf}.}

\item{ref}{An object inheriting from \code{nfd_stats}. Optional. Stats in this
object are shown as points/lines instead of boxplots.}

\item{base_units}{Optional. Used as y-axis label.}

\item{show}{Boolean. If \code{TRUE} and in interactive mode, will show the plot
in the plot window.}

\item{...}{Additional options passed to \code{\link[ggplot2:geom_point]{ggplot2::geom_point()}} and
\code{\link[ggplot2:geom_path]{ggplot2::geom_line()}}. \code{size}, \code{shape}, and \code{color} can be overridden for
points, and \code{linewidth}, \code{linetype}, and \code{color} can be overridden for
lines.}
}
\value{
\code{nfdplot} object.
}
\description{
\code{plot.nfd_stats()} plots the resulting "base" statistics after calling
\code{\link[=nfd_stats]{nfd_stats()}}. Statistics are plotted as boxplots across traces.
\code{plot.nfd_pdf()} plots the resulting PDF after calling \code{\link[=nfd_pdf]{nfd_pdf()}}. The range
of PDFs for every trace are shown as the statistics around the probability
of any given flow occurring. For monthly data, one PDF is created for each
month. Historical or some other reference trace can be shown on both plots
using \code{ref}.
}
\details{
For monthly plots, \code{plot.nfd_pdf()} will create an individual plot for all
months that exist in the \code{nfd_pdf} data frame.
}
\seealso{
\code{\link[=nfd_stats]{nfd_stats()}}, \code{\link[=save_nfdplot]{save_nfdplot()}}

\code{\link[=nfd_pdf]{nfd_pdf()}}
}
