\name{plotScreen}
\alias{plotScreen}

\title{Plot a false-color representation of screening data}

\description{
  This function is obsolete, please use \code{ggplot} with \code{geom_raster} and 
  \code{facet_wrap} instead, as described in the vignette of the \code{splots} package.
  
  \code{plotScreen} produces a false-color representation of screening
  data from multiple plates (typical use case: dozens of plates)
  in a single plot.
}

\usage{
plotScreen(z,
           ncol = 6L,
           zrange,
           main = "",
           do.names = TRUE,
           do.legend = TRUE,
           legend.label = deparse(substitute(z)),
           nx = 24,
           ny = 16,
           fill, 
           na.fill = "grey" ,
           do.grid.newpage = TRUE)
}

\arguments{
  \item{z}{A list of plate data.
    Each element provides data for a plate and is expected to be a
    vector of length at least \code{nx*ny} (only the first \code{nx*ny} elements will be displayed).
    The values are assumed to come in row-by-row order, e.g. A1, A2, A3,
    ..., B1, B2, ...
    Alternatively, the elements of \code{z} can be factors that represent
    well or spot classes (e.g. the different types of controls, empty
    wells, wells with reagent of interest). }
  \item{ncol}{Integer of length 1. Number of columns (plates in one row) for the plot.}
  \item{zrange}{If the elements of \code{z} are numeric, then this
    optional parameter can be used to specify the numeric range that is
    mapped to the color scale; by default, a symmetric interval
    around the median of \code{unlist(z)} is used.
    If the elements of \code{z} are factors, then this optional
    parameter can be used to specify the total ordered set of factor
    levels; by default, the union of the levels of the elements of
    \code{z} is used.}
  \item{fill}{If the elements of \code{z} are numeric, then this
    parameter can be used to specify a character vector of color names
    used to create a color ramp for the false color data
    representation. The default
    colors correspond to blue for low values, white for the middle of
    the range and red for high values.
    If the elements of \code{z} are factors, then this parameter can be
    used to define the corresponding color names; by default, a
    palette from \code{RColorBrewer} is used.
  }
  \item{main}{Character of length 1 or plotmath expression used for the plot title. To omit, set
    this argument to length 0, to the empty character \code{""}, to \code{FALSE}
    or to \code{NULL}.}
  \item{do.names}{Logical specifiying if plate captions should be plotted.}
  \item{do.legend}{Logical specifying if a legend for false color
    values should be output.}
  \item{legend.label}{Character of length 1 or plotmath expression that is printed
    next to the legend (see vignette for examples). To omit, set
    this argument to length 0, to the empty character \code{""}, to \code{FALSE}
    or to \code{NULL}.}
  \item{nx}{An integer for the number of columns in a plate. Default 24 for a 384-well plate.}
  \item{ny}{An integer for the number of rows in a plate. Default 16 for a 384-well plate.}
  \item{na.fill}{Color for NA values.}
  \item{do.grid.newpage}{Logical, if \code{TRUE}, then the function
  \code{grid.newpage} is called at the begin of this function.}
}

\value{None. The function is called for its side effect.}

\author{Wolfgang Huber, based on an earlier version by Oleg Sklyar.}

\examples{
  screen = lapply(1:12, function(i) cumsum(rnorm(384)))
  plotScreen(screen, ncol=3, fill=c("white", "darkblue"))
}

\keyword{dplot}
\keyword{array}

