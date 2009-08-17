plotScreen <- function(z,
                       ncol = 6L,
                       dataSlot,
                       zrange,
                       main = "",
                       do.names = TRUE,
                       do.legend = FALSE,
                       legend.mapping = seq(from=zrange[1], to=zrange[2]),
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
      stop("'fill' must have length >=2 since it is used to compute the color ",
           "ramp that represent the values in z.")

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
      z = lapply(z, function(v) { v[v<zrange[1]]=zrange[1]; v[v>zrange[2]]=zrange[2]; return(v) })
    } else {
      zrange = range(unlist(z), na.rm=TRUE)
    }
    
    nrow <- ceiling(length(z) / ncol)
    colRamp <- colorRamp(fill)

    if(is.null(names(z)))
      names(z) = paste(seq(along=z))

    dx <- if (do.legend) 0.9/ncol else 1.0/ncol
    dy <- 1.0 / nrow
    
    w <- 1.0 / nx
    h <- 1.0 / ny
    x <- (1:nx) / nx - w/2
    x <- rep(x, ny)
    y <- (1:ny) / ny - h / 2
    y <- rep(y, each=nx)

    pushViewport(viewport(width=1, height=0.92))
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
          mcol[is.na(zval), ] = 0 ## 'rgb' does not like NA
          col = do.call(rgb, lapply(1:3, function(j) mcol[,j]))
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
            grid.text(names(z)[[index]], gp = gpar(cex = 1))
            popViewport()
          }
        }
      }
    }

    if(do.legend) {
      pushViewport(viewport(x=0.95, y=0.5, width=0.09, height=1))
      zval = (0:50)/50
      mcol = colRamp(zval) / 256
      mcol[is.na(zval), ] = 0 ## 'rgb' does not like NA
      col = do.call(rgb, lapply(1:3, function(j) mcol[,j]))
      col[is.na(zval)] = na.fill
      # plot legend box
      grid.rect(0.2, zval/1.5+0.2, 0.3, 0.02/1.5, gp=gpar(col=col, fill=col))
      # print legend text: find labels
      ys = pretty(zval*diff(zrange)+min(zrange),8)
      ys = ys[ys>=zrange[1] & ys<=zrange[2]]
      zval = (ys-min(zrange))/diff(zrange)
      grid.rect(0.37, 0.5/1.5+0.2, 1e-3, 1/1.5, gp=gpar(fill="black"))
      grid.rect(0.39, zval/1.5+0.2, 0.03, 1e-3, gp=gpar(fill="black"))
      grid.text(legend.mapping[as.integer(ys)], 0.44,
                zval/1.5+0.2, gp = gpar(cex=0.7,fill="black"),
                just="left")
      popViewport()
    }
    
    if(main!=""){
      popViewport()
    }
    popViewport()
    
    invisible(NULL)
}
