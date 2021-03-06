% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colormap.R
\name{CMPar}
\alias{CMPar}
\title{Color Map Parameters}
\usage{
CMPar(
  dmin = NULL,
  dmax = NULL,
  brewer.name = NULL,
  brewer.n = 3,
  colorspace.name = NULL,
  colorspace.n = 2,
  cmap = NULL,
  label2color = NULL,
  use.data = FALSE,
  stop.points = NULL,
  na.color = "#C0C0C0",
  rev = FALSE,
  grey.scale = FALSE
)
}
\arguments{
\item{dmin}{minimum for continuous color map}

\item{dmax}{maximum for continuous color map}

\item{brewer.name}{palette name for RColorbrewer}

\item{brewer.n}{number of stop points in RColorBrewer for continuous color map}

\item{colorspace.name}{colorspace name}

\item{colorspace.n}{number of stops in colorspace palettes}

\item{cmap}{customized colormap name}

\item{label2color}{a named vector or list that defines label to color mapping
explicitly for discrete color mapping}

\item{use.data}{use data as color, data must be either common color names
or hexdecimal color names}

\item{stop.points}{custome stop points}

\item{na.color}{color for NA}

\item{rev}{reverse stop points}

\item{grey.scale}{whether to use grey scale}
}
\value{
an object of class CMPar
}
\description{
Create color map parameters
}
\examples{
WHeatmap(matrix(rnorm(2000),nrow=40)) +
  WColorBarV(1:20, cmp=CMPar(brewer.name = 'RdBu'), RightOf())
}
