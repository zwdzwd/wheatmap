#' WPlot
#'
#' WPlot
#'
#' @param hm an object of class WHeatmap
#' @return \code{NULL}
#' @export
WPlot <- function(x, ...) {
  UseMethod('WPlot', x)
}

#' WPlot
#'
#' WPlot
#'
#' @import grid
#' @export
WPlot.list <- function(obs, mar=c(0.03,0.03,0.03,0.03)) {

  mar.bottom = mar[1]
  mar.left = mar[2]
  mar.top = mar[3]
  mar.right = mar[4]

  left <- min(sapply(obs, function(x) x$dm$left))
  right <- max(sapply(obs, function(x) x$dm$left+x$dm$width))
  bottom <- min(sapply(obs, function(x) x$dm$bottom))
  top <- max(sapply(obs, function(x) x$dm$bottom+x$dm$height))
  width <- right-left
  height <- top-bottom

  ## cat(bottom, '\t', left, '\t', top, '\t', right, '\n')

  ## resize margin to accomodate texts/labels
  text.dms <- lapply(obs, CalcTextRanges)
  mar.bottom <- mar.bottom + bottom - min(sapply(text.dms, function(x) x$bottom))
  mar.left <- mar.left + left - min(sapply(text.dms, function(x) x$left))
  mar.top <- mar.top + max(sapply(text.dms, function(x) x$top)) - top
  mar.right <- mar.right + max(sapply(text.dms, function(x) x$right)) - right

  ## cat(str(text.dms),'\n')
  ## cat(mar.bottom, '\t', mar.left, '\t', mar.top, '\t', mar.right, '\n')

  library(grid)
  grid.newpage()
  for(ob in obs) {
    ## scale object
    ob$dm$left <- mar.left + (ob$dm$left-left) * (1-mar.left-mar.right) / width
    ob$dm$bottom <- mar.bottom + (ob$dm$bottom-bottom) * (1-mar.top-mar.bottom) / height
    ob$dm$width <- ob$dm$width * (1-mar.left-mar.right) / width
    ob$dm$height <- ob$dm$height * (1-mar.top-mar.bottom) / height

    ## plot
    WPlot(ob)
  }
}


#' Top of
#'
#' Generate dimension top of another object
#'
#' @param x an object with dimension
#' @param height the height of the new object (when NULL, set to proportional to data)
#' @param pad padding between the target and current
#' @param min.ratio minimum ratio of dimensions when auto-scale
#' @return a dimension generator on top of x
#' @export
TopOf <- function(x, height=NULL, pad=0.01, min.ratio=0.02) {
  force(x)
  force(height)
  force(pad)
  force(min.ratio)
  function(nr, nc) {
    if (is.null(height)) {
      .height <- 1 / x$nr * nr
      .height <- max(min.ratio, .height)
      .height <- min(1/min.ratio, .height)
      .height <- .height * x$dm$height
    } else {
      .height <- height
    }
    WDim(x$dm$left, x$dm$bottom+pad+x$dm$height, x$dm$width, .height)
  }
}

#' Beneath
#'
#' Generate dimension beneath another object
#'
#' @param x an object with dimension
#' @param height the height of the new object (when NULL set proportional to the data)
#' @param pad padding between the target and current
#' @param min.ratio minimum ratio of dimensions when auto-scale
#' @return a dimension generator beneath x
#' @export
Beneath <- function(x, height=NULL, pad=0.01, min.ratio=0.02) {
  force(x)
  force(height)
  force(pad)
  force(min.ratio)
  function(nr, nc) {
    if (is.null(height)) {
      .height <- 1 / x$nr * nr
      .height <- max(min.ratio, .height)
      .height <- min(1/min.ratio, .height)
      .height <- .height * x$dm$height
    } else {
      .height <- height
    }
    WDim(x$dm$left, x$dm$bottom-pad-.height, x$dm$width, .height)
  }
}

#' LeftOf
#'
#' Generate dimension to the left of another object
#'
#' @param x an object with dimension
#' @param width the width of the new object (when NULL, set proportional to data)
#' @param pad padding between the target and current
#' @param min.ratio minimum ratio of dimensions when auto-scale
#' @return a dimension to the left of x
#' @export
LeftOf <- function(x, width=NULL, pad=0.01, min.ratio=0.02) {
  force(x)
  force(width)
  force(pad)
  force(min.ratio)
  function(nr, nc) {
    if (is.null(width)) {
      .width <- 1 / x$nc * nc
      .width <- max(min.ratio, .width)
      .width <- min(1/min.ratio, .width)
      .width <- .width * x$dm$width
    } else {
      .width <- width
    }
    WDim(x$dm$left-pad-.width, x$dm$bottom, .width, x$dm$height)
  }
}

#' RightOf
#'
#' Generate dimension to the right of another object
#'
#' @param x an object with dimension
#' @param width the width of the new object (when NULL, set proportional to data)
#' @param pad padding between the target and current
#' @param min.ratio minimum ratio of dimensions when auto-scale
#' @return a dimension to the right of x
#' @export
RightOf <- function(x, width=NULL, pad=0.01, min.ratio=0.02) {
  force(x)
  force(width)
  force(pad)
  force(min.ratio)
  function(nr, nc) {
    if (is.null(width)) {
      .width <- 1 / x$nc * nc
      .width <- max(min.ratio, .width)
      .width <- min(1/min.ratio, .width)
      .width <- .width * x$dm$width
    } else {
      .width <- width
    }
    WDim(x$dm$left+pad+x$dm$width, x$dm$bottom, .width, x$dm$height)
  }
}
