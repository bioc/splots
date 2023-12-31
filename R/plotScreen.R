plotScreen = function(z,
                      ncol = 6L,
                      zrange,
                      main = "",
                      do.names = TRUE,
                      do.legend = TRUE,
                      legend.label = deparse(substitute(z)),
                      nx = 24,
                      ny = 16,
                      fill,
                      na.fill = "grey",
                      do.grid.newpage = TRUE) {

  
  .Deprecated(new = "ggplot, geom_raster, facet_wrap", 
              package = NULL, 
              msg = "The function splots::plotScreen is obsolete, please use ggplot with geom_raster and facet_wrap instead, as described in the vignette of the splots package",
              old = as.character(sys.call(sys.parent()))[1L])
  
  ##--------------------------------------------------
  ## Check arguments
  ##--------------------------------------------------

  ## z:
  if (missing(z))
    stop("'z' must not be missing.")
  if (!is.list(z))
    stop("'z' must be a list.")
  if (length(z) < 1L)
    stop("'z' must have length >= 1.")
    
  ## ncol:
  if (!((length(ncol)==1L)&&(ncol>0L)&&!is.na(ncol)))
    stop("'ncol' must be a positive integer of length 1.")

  ## nx, ny
  if (!((length(nx)==1L)&&(nx>0L)&&!is.na(nx)))
    stop("'nx' must be a positive integer of length 1.")
  if (!((length(ny)==1L)&&(ny>0L)&&!is.na(ny)))
    stop("'ny' must be a positive integer of length 1.")

  ## do.names, do.legend
  if(!(is.logical(do.names) && (length(do.names)==1L) && !is.na(do.names)))
    stop("'do.names' must be logical of length 1.")
  if(!(is.logical(do.legend) && (length(do.legend)==1L) && !is.na(do.legend)))
    stop("'do.legend' must be logical of length 1.")

  ## legend.label
  if(do.legend){
    if(length(legend.label)>1L)
      stop("'legend.label' must have length <=1.")
    have.legend.label = !( is.null(legend.label) || (length(legend.label)==0) ||
                           identical(legend.label, "") || identical(legend.label, FALSE) )
    if(have.legend.label)
      if(!(is.character(legend.label) || is.expression(legend.label)))
        stop("'legend.label' must be a character or expression.")
  }

  ## main
  if(length(main)>1L)
    stop("'main' must have length <=1.")
  have.main = !( is.null(main) || (length(main)==0) ||
                 identical(main, "") || identical(main, FALSE) )
  if(have.main)
    if(!(is.character(main) || is.expression(main)))
      stop("'main' must be a character or expression.")
  
  ##------------------------------------------------------------------------------
  ## Figure out the type of the elements of z: They should all be the same, factor
  ## or numeric. The following block of code defines (if missing) zrange, fill
  ## and 'cleans up' z.
  ##-------------------------------------------------------------------------------
  if(is.numeric(z[[1]])) {
    ## --- z is numeric ---
    wh = which(!sapply(z, is.numeric))
    if(length(wh)>0)
      stop(sprintf("The elements of 'z' should all have the same type. Element 1 is numeric, but %d is not.\n"), wh[1])
      
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
      zrange = med + c(-1, +1) * max(abs(values-med), na.rm=TRUE)
    }

    if(missing(fill))
      fill = c("blue", "white", "red")
    colRamp = colorRamp(fill)
      
  } else if (is.factor(z[[1]])) {
    ## --- z is factor ---
    wh = which(!sapply(z, is.factor))
    if(length(wh)>0)
      stop(sprintf("The elements of 'z' should all have the same type. Element 1 is factor, but %d is not.\n"), wh[1])
    levs = levels(unlist(z))
    if(!missing(zrange)){
      if(!all(levs %in% zrange))
        stop("'z' is a list of factors, but it contains levels that are not contained in 'zrange'.")
    } else {
      zrange = levs
    }
    
    if(missing(fill))
      fill = rep(brewer.pal(9, "Set1"), ceiling(length(zrange)/9))[seq(along=zrange)]
      
  }
  
  if(is.null(names(z)))
    names(z) = paste(seq(along=z))

  nrow = ceiling(length(z) / ncol)
  px = rep(seq_len(nx), ny)
  py = rep(seq_len(ny), each=nx)
    
  if(do.grid.newpage)
    grid.newpage()

  mainViewportHeight = unit(1, "npc")
  mainViewportWidth  = unit(1, "npc")

  if (have.main) {
    mainViewportHeight = mainViewportHeight - unit(2, "strheight", data="!") 
    grid.text(main, x=0.5, y=1, hjust=0.5, vjust=1.5)
  }

  ##--------------------------------------------------
  ## legend
  ##--------------------------------------------------
  if(do.legend) {
      
    if(is.numeric(zrange)){
      yPos  = pretty(zrange, 10)
      yPos  = yPos[-c(1L, length(yPos))]
      yText = paste(yPos)
      
      steps = (seq(yPos[1], yPos[length(yPos)], length=50) - zrange[1]) / diff(zrange)
      mcol = colRamp(steps) / 256
      col = do.call(rgb, lapply(1:3, function(j) mcol[,j]))
      
    } else {
      yPos  = seq(along=zrange)
      yText = zrange
      col = fill
    }
    
    dyPos = yPos[2]-yPos[1]

    widths = c(space = 1, bar = 2, tick = max(nchar(yText))+1, label = 4)
    unitWidth = unit(sum(widths[1 : if(have.legend.label) 4 else 3 ]), "strwidth", data="x")
    mainViewportWidth  = mainViewportWidth - unitWidth
    
    pushViewport(
                 viewport(
                          x = unit(1, "npc"),
                          width = unitWidth,
                          y = unit(0.5, "npc"),
                          height = unit(0.7, "npc"),
                          just = c("right", "center"),                      
                          xscale = c(0, sum(widths)),                      
                          yscale = c(yPos[1] - dyPos, yPos[length(yPos)])
                          )
                 )
    
    grid.raster(matrix(rev(col), ncol=1, nrow=length(col)),
                interpolate = FALSE,
                x = unit(widths["space"], "native"), width  = unit(widths["bar"], "native"), 
                y = unit(0, "npc"), height = unit(1, "npc"),
                just=c("left", "bottom"))
    
    grid.text(label = yText,
              x = sum(widths[c("space", "bar")]),
              y = yPos - dyPos/2,
              default.units = "native", just=c("left", "center"),
              gp = gpar(cex=1, fill="black"))

    if(have.legend.label)
      grid.text(label = legend.label,
              x = unit(1, "npc") - unit(0.5, "strwidth", data="x"),
              y = unit(0.5, "npc"),
              default.units="native", just=c("center", "bottom"),
              gp = gpar(cex=1, fill="black"), rot=90)
        
    popViewport()
  }
 

  ##--------------------------------------------------
  ## plates
  ##--------------------------------------------------
  pushViewport(
               viewport(
                        x      = 0,
                        width  = mainViewportWidth,
                        y      = 0,
                        height = mainViewportHeight,
                        just   = c("left", "bottom"),
                        xscale = c(0, ncol),
                        yscale = c(0, nrow)
                        )
               )
                 
  plateHeight = unit(0.95, "native")
  if (do.names)
    plateHeight = plateHeight - unit(2, "strheight", data="!")
  
  for (i in seq_len(ncol)) {
    for (j in seq_len(nrow)) {
      index = (j-1)*ncol + i
      if (index <= length(z)) {
        if(is.numeric(zrange)){
          zval = (z[[index]] - zrange[1]) / diff(zrange)
          zval = ifelse(zval<0, 0, ifelse(zval>1, 1, zval)) ## robustness against rounding errors
          mcol = colRamp(zval) / 256
          mcol[is.na(zval), ] = 0 ## 'rgb' does not like NA
          col = do.call(rgb, lapply(1:3, function(j) mcol[,j]))
          col[is.na(zval)] = na.fill
        } else {
          col = fill[as.integer(z[[index]])]
        }
        grid.raster(matrix(col, nrow=ny, ncol=nx, byrow=TRUE),
                    x=i-0.5, y=nrow-j+1, width=0.95, height=plateHeight,
                    interpolate=FALSE, default.units="native",
                    just=c("center", "top"))
        
        if (do.names)
          grid.text(names(z)[[index]],
                    x = unit(i-0.5, "native"), y = unit(nrow-j, "native") + unit(1, "strheight", data="!"),
                    just=c("center", "bottom"))
      } ## if
    } ## for j
  } # for i
  popViewport()
}
