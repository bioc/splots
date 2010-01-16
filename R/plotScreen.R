plotScreen <- function(z,
                       ncol = 6L,
                       zrange,
                       main = "",
                       do.names = TRUE,
                       do.legend = TRUE,
                       nx = 24,
                       ny = 16,
                       fill = c("blue", "white", "red"), 
                       abris = "#333333",
                       na.fill = "grey",
                       do.grid.newpage = TRUE) { 
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
    
    for(i in seq(along=z))
      z[[i]][!is.finite(z[[i]])] = NA

    if(!missing(zrange)){
      zrange = sort(zrange)
      if((length(zrange)!=2) || (!all(is.finite(zrange))) || (zrange[1]>=zrange[2]))
        stop("'zrange' must be of length 2 with finite values.")
      z = lapply(z, function(v) { v[v<zrange[1]]=zrange[1]; v[v>zrange[2]]=zrange[2]; return(v) })
    } else {
      values = unlist(z)
      med = median(values, na.rm=TRUE)
      zrange = med+c(-1,1) * max( max(values, na.rm=TRUE)-med,
                                  med-min(values, na.rm=TRUE) )
    }
    
    colRamp = colorRamp(fill)

    if(is.null(names(z)))
      names(z) = paste(seq(along=z))

    nrow = ceiling(length(z) / ncol)
    px = rep(seq_len(nx), ny)
    py = rep(seq_len(ny), each=nx)
    
    if(do.grid.newpage)
      grid.newpage()

    mainViewportHeight = unit(1, "npc")
    mainViewportWidth  = unit(1, "npc")

    if (main!="") {
      mainViewportHeight = mainViewportHeight - unit(2, "strheight", data="!") 
      grid.text(main, x=0.5, y=1, hjust=0.5, vjust=1.5)
    }

    if(do.legend) {
      ys = pretty(zrange, 10)
      ys = ys[-c(1,length(ys))]
      barWidth  = 8
      textWidth = max(nchar(paste(ys)))+2
      unitWidth = unit(barWidth+textWidth, "strwidth", data="x")
      mainViewportWidth  = mainViewportWidth - unitWidth
      
      steps = (seq(ys[1], ys[length(ys)], length=50) - zrange[1]) / diff(zrange)
      mcol = colRamp(steps) / 256
      col = do.call(rgb, lapply(1:3, function(j) mcol[,j]))
      
      pushViewport(viewport(
          x = unit(1, "npc"),
          width = unitWidth,
          y = unit(0.5, "npc"),
          height = unit(0.7, "npc"),
          just = c("right", "center"),                      
          xscale = c(0, barWidth+textWidth),                      
          yscale = ys[c(1, length(ys))] ))

      grid.raster(matrix(col, ncol=1, nrow=length(col)),
                  interpolate = FALSE,
                  x = 2, width = barWidth-2, 
                  y = zrange[1], height=diff(zrange),
                  default.units="native", just=c("left", "bottom"))
                 
      grid.text(label = paste(ys),
                x = barWidth,
                y = ys,
                default.units="native", just=c("left", "bottom"),
                gp = gpar(cex=1, fill="black"))
      
      popViewport()
    }
 
    pushViewport(viewport(
      x      = 0,
      width  = mainViewportWidth,
      y      = 0,
      height = mainViewportHeight,
      just   = c("left", "bottom"),
      xscale = c(0, ncol),
      yscale = c(0, nrow)))
                 
    plateHeight = unit(0.95, "native")
    if (do.names)
      plateHeight = plateHeight - unit(2, "strheight", data="!")
    
    for (i in seq_len(ncol)) {
      for (j in seq_len(nrow)) {
        index = (j-1)*ncol + i
        if (index <= length(z)) {
          zval = (z[[index]] - zrange[1]) / diff(zrange)
          zval = ifelse(zval<0, 0, ifelse(zval>1, 1, zval)) ## robustness against rounding errors
          mcol = colRamp(zval) / 256
          mcol[is.na(zval), ] = 0 ## 'rgb' does not like NA
          col = do.call(rgb, lapply(1:3, function(j) mcol[,j]))
          col[is.na(zval)] = na.fill
    
          grid.raster(matrix(col, nrow=nx, ncol=ny, byrow=TRUE),
                      x=i, y=nrow-j+1, width=0.95, height=plateHeight,
                      interpolate=FALSE, default.units="native",
                      just=c("right", "top"))

          if (do.names)
            grid.text(names(z)[[index]],
                      x = unit(i-0.5, "native"), y = unit(nrow-j, "native") + unit(1, "strheight", data="!"),
                      just=c("center", "bottom"))
        } ## if
      } ## for j
    } # for i
    popViewport()
}
