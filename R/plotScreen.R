plotScreen <- function(z,
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
                       na.fill = "grey") { 
    if (missing(z))
      stop("'z' must not be missing.")
    if (!is.list(z))
      stop("'z' must be a list.")
    if (length(z) < 1L)
        stop("'z' must have length >= 1.")
    
    if(length(fill) < 2L)
      stop("'fill' must have length >=2 since it is used to compute the color ramp that represent the values in z.")

    if (!((length(ncol)==1L)&&(ncol>0L)&&!is.na(ncol)))
      stop("'ncol' must be a positive integer of length 1.")
    if (!((length(nx)  ==1L)&&(  nx>0L)&&!is.na(nx)))
      stop("'nx' must be a positive integer of length 1.")
    if (!((length(ny)  ==1L)&&(  ny>0L)&&!is.na(ny)))
      stop("'ny' must be a positive integer of length 1.")

    if(!(is.logical(do.names) && (length(do.names)==1L) && !is.na(do.names)))
      stop("'do.names' must be logical of length 1.")
    if(!(is.logical(do.legend) && (length(do.legend)==1L) && !is.na(do.legend)))
      stop("'do.legend' must be logical of length 1.")
    
    if(!missing(dataSlot))
      z = lapply(z, "[[", dataSlot)

    for(i in seq(along=z))
      z[[i]][!is.finite(z[[i]])] = NA

    if(!missing(zrange)){
      zrange = sort(zrange)
      if((length(zrange)!=2) || (!all(is.finite(zrange))) || (zrange[1]>=zrange[2]))
        stop("'zrange' must be of length 2 with finite values.")
    } else {
      zrange = range(unlist(z), na.rm=TRUE)
    }
    
    nrow <- ceiling(length(z) / ncol)
    colRamp <- colorRamp(fill)

    if(is.null(names(z)))
      names(z) = paste(seq(along=z))

    dx <- 1.0 / ncol
    dy <- 1.0 / if (do.legend) (nrow + 1.0) else nrow
    
    w <- 1.0 / nx
    h <- 1.0 / ny
    x <- (1:nx) / nx - w/2
    x <- rep(x, ny)
    y <- (1:ny) / ny - h / 2
    y <- rep(y, each=nx)

    if(main!=""){
      grid.text(main, x=0.5, y=1, hjust=0.5, vjust=pi/2)
      pushViewport(viewport(x=0.5, width=1,
                            y=unit(0.5, "npc")-unit(1, "char"),
                            height=unit(1, "npc")-unit(2, "char")))
    }
    
    for (i in 1:ncol) {
      for (j in 1:nrow) {
        index = (j-1)*ncol + i
        if (index <= length(z)) {
          zval = (z[[index]] - zrange[1]) / diff(zrange)
          mcol = colRamp(zval) / 256
          col = do.call("rgb", lapply(seq_len(ncol(mcol)), function(j) mcol[,j]))
          col[is.na(zval)] = na.fill
          
          if (do.names)
            pushViewport(viewport(x = (i-0.5)*dx , y = 1-(j-0.6)*dy, width = 0.95 * dx, height = 0.8 * dy))
          else
            pushViewport(viewport(x = (i-0.5)*dx , y = 1-(j-0.5)*dy, width = 0.95 * dx, height = 0.95 * dy))

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
    }

    if(do.legend)
      warning("do.legend is not yet implemented.")
    
    if(main!=""){
      popViewport()
    }
    
    invisible(NULL)
}
