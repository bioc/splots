\name{plotScreen}

\alias{plotScreen}

\docType{function}

\title{Plot the full screen layout}

\description{
    \code{plot.screen} visualizes the whole screen in a single image preview. All wells are 
    plotted side by side, with data values shown in false colors.
}

\usage{
    plotScreen(z, ncol = 6, dataSlot = NA, zrange = range(z, na.rm=TRUE), do.names = TRUE, do.legend = TRUE, nx = 24, ny = 16, fill = c("blue", "white", "red"), abris = "#333333", na.grayOpac = 1.0)
}
\arguments{
    \item{z}{A list of plate data. Each element provides data for a single plate. Elements 
        should be named if \code{do.names} is TRUE. Data of each element can be an array or a 
        matrix of an arbitrary length, but only \code{nx*ny} elements will be displayed to 
        constitute a plate-like format. Missing values will be displayed as gray.}
    \item{ncol}{Number of columns (plates in one row) for the plot.}
    \item{dataSlot}{If elements of z are \code{data.frame} or \code{list} themselves,
        \code{dataSlot} specifies which element of those data values has to be plotted. A 
        character value.}
    \item{zrange}{The range of data to plot. By default the full data range will be plotted. 
        Please beware that if \code{dataSlot} is set, the default range can be totally wrong!}
    \item{do.names}{A boolean flag specifiying if plate captions should be plotted.}
    \item{do.legend}{A boolean flag specifying if a legend for false color values should be output.}
    \item{nx}{An integer for the number of columns in a plate. Default 24 for a 384-well plate.}
    \item{ny}{An integer for the number of rows in a plate. Default 16 for a 384-well plate.}
    \item{fill}{A character vector of colors to create a color ramp for the false color data
        representation. At least two colors must be specified. The default color correspond to
        blue for low values, white for the middle of the range and red for high values.}
    \item{abris}{A character value for the color of the plate outline.}
    \item{na.grayOpac}{A double value for the opacity of gray for NA values.}
}

\value{
    invisible(NULL)
}

\references{
    The initial idea of the function goes to Ligia Bras, the author of the 'cellHTS' package of Bioconductor.
}

\author{
    Copyright (c) 2005-2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

\examples{
    data(splots.screen)
    plotScreen(screen, 3)
}

\keyword{dplot}
\keyword{array}

