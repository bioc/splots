# -------------------------------------------------------------------------
# plot.screen -- plots a full screen layout of a given number of plates in one image
 
# Copyright (c) 2006 Oleg Sklyar

# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License 
# as published by the Free Software Foundation; either version 2.1
# of the License, or (at your option) any later version.          

# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

# See the GNU Lesser General Public License for more details.
# LGPL license wording: http://www.gnu.org/licenses/lgpl.html

# -------------------------------------------------------------------------
plotScreen <- function(z, ncol = 6, dataSlot = NA, zrange = range(z, na.rm=TRUE), do.names = TRUE, do.legend = TRUE,
                        nx = 24, ny = 16, fill = c("blue", "white", "red"), 
                        abris = "#333333", na.grayOpac = 1.0) { # fill -> brewer.pal(9,"YlGnBu")
    if (missing(z))
        stop("argument 'z' is essential and must be a list")
    if (class(z) != "list")
        stop("argument 'z' must be a list (list of data per plate)")
    if (length(z) < 1)
        stop("at least one dataset must be supplied")
    if(length(fill) < 2)
        stop("the value of z is represented by colour in the resulting image, therefore you must specify at least two colours for the ramp")
    ncol <- as.integer(ncol)
    if (ncol <= 0)
        stop("ncol must be a positive integer, 1 or larger")
    if (diff(zrange) == 0)
        stop("please specify a positive (non-zero) zrange")
    nrow <- ceiling(length(z) / ncol)
    colRamp <- colorRamp(fill)
    if (is.null(names(z))) {
        if (do.names)
            warning("names(z) should not be NULL with do.names set to TRUE")
        do.names <- FALSE
    }
    dx <- 1.0 / ncol
    if (do.legend)
        dy <- 1.0 / (nrow + 1)
    else
        dy <- 1.0 / nrow
    w <- 1.0 / nx
    h <- 1.0 / ny
    x <- (1:nx) / nx - w/2
    x <- rep(x, ny)
    y <- (1:ny) / ny - h / 2
    y <- rep(y, each=nx)
    device <- names(dev.cur())
    if (!device %in% c("pdf", "postscript"))
        grid.newpage()
    for (i in 1:ncol)
        for (j in 1:nrow) {
            index <- (j - 1) * ncol + i
            if (index <= length(z)) {
                if (!is.na(dataSlot))
                    zval <- (z[[index]][dataSlot] - zrange[1]) / diff(zrange)
                else
                    zval <- (z[[index]] - zrange[1]) / diff(zrange)
                if (class(zval) == "data.frame")
                    zval <- as.double(zval[,1])
                else
                if (class(zval) == "list")
                    zval <- as.double(unlist(zval))
                else
                    zval <- as.double(zval)
                mcol = colRamp(zval) / 256
                mcol[is.na(mcol)] = 1 - na.grayOpac
                col = rgb(mcol[,1], mcol[,2], mcol[,3])
                # plotting images
                if (do.names)
                    pushViewport(viewport(x = (i - 0.5) * dx , y = 1 - (j - 0.6) * dy, width = 0.95 * dx, height = 0.8 * dy))
                else
                    pushViewport(viewport(x = (i - 0.5) * dx , y = 1 - (j - 0.5) * dy, width = 0.95 * dx, height = 0.95 * dy))
                grid.rect(x, 1 - y, w, h, gp = gpar(col = col, fill = col))
                grid.rect(0.5, 0.5, 1, 1, gp = gpar(col = abris, fill = "transparent"))
                popViewport()
                if (do.names) {
                    pushViewport(viewport(x = (i - 0.5) * dx, y = 1 - (j - 0.1) * dy, width = 0.95 * dx, height = 0.2 * dy))
                    grid.text(names(z)[[index]], gp = gpar(cex = 0.6))
                    popViewport()
                }
            }
        }
    invisible(NULL)
}
