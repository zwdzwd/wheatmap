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

  left <- min(sapply(obs, function(x) x$dim[1]))
  right <- max(sapply(obs, function(x) x$dim[1]+x$dim[3]))
  bottom <- min(sapply(obs, function(x) x$dim[2]))
  top <- max(sapply(obs, function(x) x$dim[2]+x$dim[4]))
  width <- right-left
  height <- top-bottom

  ## cat(bottom, '\t', left, '\t', top, '\t', right, '\n')

  ## resize margin to accomodate texts/labels
  text.dims <- lapply(obs, CalcTextRanges)
  mar.bottom <- mar.bottom + bottom - min(sapply(text.dims, function(x) x$bottom))
  mar.left <- mar.left + left - min(sapply(text.dims, function(x) x$left))
  mar.top <- mar.top + max(sapply(text.dims, function(x) x$top)) - top
  mar.right <- mar.right + max(sapply(text.dims, function(x) x$right)) - right

  ## cat(str(text.dims),'\n')
  ## cat(mar.bottom, '\t', mar.left, '\t', mar.top, '\t', mar.right, '\n')

  library(grid)
  grid.newpage()
  for(ob in obs) {
    ## scale object
    ob$dim[1] <- mar.left + (ob$dim[1]-left) * (1-mar.left-mar.right) / width
    ob$dim[2] <- mar.bottom + (ob$dim[2]-bottom) * (1-mar.top-mar.bottom) / height
    ob$dim[3] <- ob$dim[3] * (1-mar.left-mar.right) / width
    ob$dim[4] <- ob$dim[4] * (1-mar.top-mar.bottom) / height

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
  function(y) {
    if (is.null(height)) {
      .height <- 1/x$nr*y$nr
      .height <- max(min.ratio, .height)
      .height <- min(1/min.ratio, .height)
      .height <- .height * x$dim[4]
    } else {
      .height <- height
    }
    c(x$dim[1], x$dim[2]+pad+x$dim[4], x$dim[3], .height)
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
  function(y) {
    if (is.null(height)) {
      .height <- 1/x$nr*y$nr
      .height <- max(min.ratio, .height)
      .height <- min(1/min.ratio, .height)
      .height <- .height * x$dim[4]
    } else {
      .height <- height
    }
    c(x$dim[1], x$dim[2]-pad-.height, x$dim[3], .height)
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
  function(y) {
    if (is.null(width)) {
      .width <- 1/x$nc*y$nc
      .width <- max(min.ratio, .width)
      .width <- min(1/min.ratio, .width)
      .width <- .width * x$dim[3]
    } else {
      .width <- width
    }
    c(x$dim[1]-pad-.width, x$dim[2], .width, x$dim[4])
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
  function(y) {
    if (is.null(width)) {
      .width <- 1/x$nc*y$nc
      .width <- max(min.ratio, .width)
      .width <- min(1/min.ratio, .width)
      .width <- .width * x$dim[3]
    } else {
      .width <- width
    }
    c(x$dim[1]+pad+x$dim[3], x$dim[2], .width, x$dim[4])
  }
}

#' Class WObject
#'
#' Class WObject
#'
#' @param dim dimension
#' @param nr number of rows
#' @param nc number of columns
#' @return an object of class WObject
#' @export
WObject <- function(dim=NULL, nr=NULL, nc=NULL) {
  o <- list(nc=nc, nr=nr, dim=dim)
  o
}

#' Row-bind plotting objects
#'
#' Row-bind plotting objects
#'
#' @param ... plotting objects
#' @return an object of class WObject
#' @export
RBind <- function(..., nr=NULL) {
  obs <- list(...)
  if (is.null(nr))
    nr <- min(sapply(obs, function(o) o$nr))
  nc <- sum(sapply(obs, function(o) o$nc))
  dim <- c(0,0,0,0)
  dim[1] <- min(sapply(obs, function(o) o$dim[1]))
  dim[2] <- min(sapply(obs, function(o) o$dim[2]))
  dim[3] <- max(sapply(obs, function(o) o$dim[1]+o$dim[3]))-dim[1]
  dim[4] <- max(sapply(obs, function(o) o$dim[2]+o$dim[4]))-dim[2]
  WObject(dim=dim, nc=nc, nr=nr)
}




