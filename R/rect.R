#' construct a WRect
#' 
#' @param x a plotting object or its name
#' @param x.span x-axis/horizontal span (e.g., c(2,4))
#' @param y.span y-axis/vertical span (e.g., c(5,9))
#' @param color edge color
#' @param lwd edge width
#' @param fill fill color
#' @param name name
#' @return a WRect object
#' 
#' @export
WRect <- function(x=NULL, x.span=NULL, y.span=NULL, color='black', lwd=3, fill=NA, name='') {

  rect <- lapply(formals(), eval)
  invisible(lapply(names(as.list(match.call()))[-1], function (nm) {
    rect[[nm]] <<- get(nm)
  }))
  class(rect) <- c('WRect', 'WAnnotate', 'WObject')
  force(rect)
  structure(function(group) {
    if (is.null(rect$x))
      rect$x <- group$children[[length(group$children)]]$name

    x <- Resolve(rect$x, group)
    if (is.null(x.span))
      rect$x.span <- c(1, x$dm$nc)
    if (is.null(y.span))
      rect$y.span <- c(1, x$dm$nr)

    ## make inclusive
    rect$x.span[1] <- rect$x.span[1]-1
    rect$y.span[1] <- rect$y.span[1]-1

    rect$x.span <- rect$x.span/x$dm$nc
    rect$y.span <- (x$dm$nr-rect$y.span)/x$dm$nr

    rect$x.span[1] <- XToTop(x, group, rect$x.span[1])
    rect$x.span[2] <- XToTop(x, group, rect$x.span[2])
    rect$y.span[1] <- YToTop(x, group, rect$y.span[1])
    rect$y.span[2] <- YToTop(x, group, rect$y.span[2])

    rect$dm <- WDim(rect$x.span[1], rect$y.span[2],
                    rect$x.span[2] - rect$x.span[1],
                    rect$y.span[1] - rect$y.span[2])
    rect
  }, class=c('WGenerator', 'WObject'))
}

#' print WRect
#'
#' @param x a WRect object
#' @param cex factor for scaling text
#' @param layout.only print layout only
#' @param stand.alone plot WRect standalone
#' @param ... additional options
#' @import grid
#' @export
print.WRect <- function(x, cex=1, layout.only=FALSE, stand.alone=TRUE, ...) {

  if (stand.alone) {
    group <- WGroup(x)
    print(group)
    return(group)
  }

  if (!layout.only) {
    grid.rect(
      x=unit(x$dm$left,'npc'), y=unit(x$dm$bottom,'npc'),
      width=unit(x$dm$width,'npc'),
      height=unit(x$dm$height,'npc'),
      just=c('left','bottom'),
      gp=gpar(col=x$color, fill=x$fill, lwd=x$lwd))
  }
}

CalcTextBounding.WObject <- function(rect, group) {
  rect$dm
}

