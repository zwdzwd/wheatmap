#' Color Map Parameters
#'
#' Create color map parameters
#'
#' @param cm existing color maps
#' @param dmin minimum for continuous color map
#' @param dmax maximum for continuous color map
#' @param brewer.name palette name for RColorbrewer
#' @param brewer.n number of stop points in RColorbrewer for continuous color map
#' @param colorspace.name colorspace name
#' @param colorspace.n number of stops in colorspace palettes
#' @param cmap customized colormap name
#' @param stop.points custome stop points
#' @param grey.scale whether to use grey scale
#' @return an object of class CMPar
#' @export
CMPar <- function(dmin = NULL, dmax = NULL, # color scale max and min
                  brewer.name='Accent', brewer.n=3,
                  colorspace.name='rainbow_hcl', colorspace.n=2,
                  cmap='jet',
                  stop.points=NULL, # color names at stop points
                  grey.scale=FALSE) {
  cmp <- lapply(formals(), eval)
  invisible(lapply(names(as.list(match.call()))[-1], function (nm) {
    cmp[[nm]] <<- get(nm)
  }))
  cmp
}

#' Constructor for ColoMap object
#'
#' Create color maps
#'
#' @param discrete whether colormap is discrete
#' @param colors colors for each data point
#' @param dmin miminum in continuous color map
#' @param dmax maximum in continuous color map
#' @param scaler scaler function from data range to 0-1
#' @param mapper function that maps data to color
#' @return an object of class ColorMap
#' @export
ColorMap <- function(continuous=TRUE,
                     colors=NULL,
                     dmin=NULL, dmax=NULL,
                     scaler=NULL, mapper=NULL) {
  cm <- lapply(formals(), eval)
  invisible(lapply(names(as.list(match.call()))[-1], function (nm) {
    cm[[nm]] <<- get(nm)
  }))
  class(cm) <- 'ColorMap'
  cm
}

#' map data to continuous color
#'
#' map data to continuous color
#'
#' @param data numeric vector
#' @param cmp an color map parameter object of class CMPar
#' @return an object of ColorMap
#' @export
MapToContinuousColors <- function(data, cmp=CMPar(), given.cm=NULL) {

  attach(cmp)
  on.exit(detach(cmp))

  if (!is.null(given.cm)) {
    given.cm$colors <- apply(
      given.cm$mapper(given.cm$scaler(data)), 1,
      function(x) do.call(rgb, c(as.list(x), maxColorValue=255)))
    return(given.cm)
  }

  if (is.null(stop.points)) {
    if (!is.null(cmap)) {
      data(colormap)
      stop.points <- get(paste0(cmap,'.stops'))
    } else if (!is.null(brewer.name)) {
      ## use display.brewer.all for the brewer colors
      library(RColorBrewer)
      ## note that brewer.n cannot be >8 typically
      if (brewer.n < 3)
        brewer.n <- 3
      stop.points <- brewer.pal(brewer.n, brewer.name)
    } else {
      library(colorspace)
      ## colorspace.name can be
      ## diverge_hcl, diverge_hsv, terrain_hcl, heat_hcl, sequential_hcl and rainbow_hcl
      ## colorspace.n can be very large
      stop.points <- get(colorspace.name)(colorspace.n)
    }
  }

  ## cap data
  if (!is.null(dmax))
    data[data>=dmax] <- dmax
  if (!is.null(dmin))
    data[data<=dmin] <- dmin

  .dmax <- max(dmax, data)
  .dmin <- min(dmin, data)
  if (.dmax==.dmin) # when range==0
    .dmax <- .dmax+1
  data <- (data - .dmin) / (.dmax-.dmin)

  cm <- ColorMap(
    dmin = .dmin, dmax = .dmax,
    scaler = function(x) {(x-.dmin)/(.dmax-.dmin)},
    mapper = colorRamp(stop.points, alpha=TRUE))
  cm$colors = apply(cm$mapper(data), 1, function(x) do.call(rgb, c(as.list(x), maxColorValue=255)))
  cm
}

#' map data to discrete color
#'
#' map data to discrete color
#'
#' @param data numeric vector
#' @param cmp an color map parameter object of class CMPar
#' @return an object of ColorMap
#' @import RColorBrewer
#' @import colorspace
#' @export
MapToDiscreteColors <- function(data, cmp=CMPar(), given.cm=NULL) {

  attach(cmp)
  on.exit(detach(cmp))

  if (!is.null(given.cm)) {
    given.cm$colors <- given.cm$mapper[as.character(data)]
    return(given.cm)
  }

  library(RColorBrewer)
  library(colorspace)
  alphabet <- as.character(unique(as.vector(data)))
  if (!is.null(brewer.name) && length(alphabet)<=brewer.pal.info[brewer.name,'maxcolors']) {
    ## use grey scale for binary and unary data
    if (length(alphabet)<3)
      mapped.colors <- c('#C0C0C0','#808080')[1:length(alphabet)]
    else
      mapped.colors <- brewer.pal(length(alphabet), brewer.name)
  } else {
    mapped.colors <- get(colorspace.name)(length(alphabet))
  }

  cm <- ColorMap(
    continuous=FALSE,
    mapper=setNames(mapped.colors, alphabet))
  cm$colors=cm$mapper[as.character(data)]
  cm
}
