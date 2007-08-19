\name{plotScreen}
\alias{plotScreen}

\docType{function}

\title{Plot a false-color representation of screening data}

\description{
  \code{plotScreen} produces a false-color representation of screening
  data from multiple plates (typical use case: dozens of plates)
  in a single plot.
}

\usage{
plotScreen(z,
           ncol = 6L,
           dataSlot,
           zrange,
           main = "",
           do.names = TRUE,
           do.legend = FALSE,
           nx = 24L,
           ny = 16L,
           fill = c("blue", "white", "red"), 
           abris = "#333333",
           na.fill = "grey")
}

\arguments{
  \item{z}{A list of plate data. Each element provides data for a
    single plate and is expected to be a vector of length at least \code{nx*ny},
    (only the first \code{nx*ny} elements will be displayed).
    The values are assumed to come in row-by-row order. The elements of
    \code{z} can also be data.frames or lists, see the argument \code{dataSlot}.}
  \item{ncol}{Integer of length 1. Number of columns (plates in one row) for the plot.}
  \item{dataSlot}{Character of length 1. This can be convenient if elements of z are of class
    \code{data.frame} or \code{list} themselves. If \code{dataSlot} is not missing, 
    \code{lapply(z, "[[", dataSlot)} is used.}
  \item{zrange}{The numeric range that is mapped to the color scale. If missing,
    the range of the data in \code{z} is used.}
  \item{main}{Character of length 1, the plot title.}
  \item{do.names}{A boolean flag specifiying if plate captions should be plotted.}
  \item{do.legend}{A boolean flag specifying if a legend for false color
    values should be output. This is currently not implemented.}
  
  \item{nx}{An integer for the number of columns in a plate. Default 24 for a 384-well plate.}
  \item{ny}{An integer for the number of rows in a plate. Default 16 for a 384-well plate.}
  \item{fill}{A character vector of colors to create a color ramp for the false color data
    representation. At least two colors must be specified. The default color correspond to
    blue for low values, white for the middle of the range and red for high values.}
  \item{abris}{A character value for the color of the plate outline.}
  \item{na.fill}{Color for NA values.}
}

\value{
    invisible(NULL)
}

\references{
    The initial idea of the function goes to Ligia Bras, the author of the 'cellHTS' package.
}

\author{
    Oleg Sklyar \email{osklyar@ebi.ac.uk}   
}

\examples{
  data("splots.screen")
  plotScreen(screen, ncol=3, main="splots example")
}

\keyword{dplot}
\keyword{array}

