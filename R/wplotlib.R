
WFlatten <- function(.obs) {
  obs <- list()
  for(o in .obs){
    if ('WGroup' %in% class(o))
      obs <- c(obs, o$obs)
    else
      obs[[length(obs)+1]] <- o
  }
  obs
}

#' Draw WGroup
#'
#' @param group plot to display
#' @import grid
#' @export
print.WGroup <- function(group, mar=c(0.03,0.03,0.03,0.03)) {

  ## flatten WGroups
  obs <- WFlatten(group$obs)

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
    plot(ob, stand.alone=FALSE)
  }
}

#' @export
plot.WGroup <- print.WGroup


.SetToDim <- function(x) {
  if (is.null(x))
    return (x)
  else if (is.character(x)) {
    x <- GetCanvas(x)
    return (x$dm)
  } else
    return (x$dm)
}

.GroupSetToDim <- function(x, group) {
  if (is.character(x)) {
    if (is.null(group))
      message('No group provided. This is a bug.')
    stopifnot(!is.null(group))
    .x <- group[x]
    if (is.null(.x))
      message('Name not found: ', x, '. Make sure objects are named.')
    x <- .x$dm
  }
  x
}

#' Top of
#'
#' Generate dimension top of another object
#'
#' @param x an object with dimension
#' @param height the height of the new object (when NULL, set to proportional to data)
#' @param pad padding between the target and current
#' @param min.ratio minimum ratio of dimensions when auto-scale
#' @param h.aln object for horizontal alignment (when NULL, set to x)
#' @param v.scale object for vertical scaling (when NULL, set to x)
#' @param v.scale.proportional when v.scale is provided, whether to make proportional to data
#' @return a dimension generator on top of x
#' @export
TopOf <- function(x, height=NULL, pad=0.01, min.ratio=0.02, h.aln=NULL, v.scale=NULL, v.scale.proportional=FALSE) {

  x <- .SetToDim(x)
  h.aln <- .SetToDim(h.aln)
  v.scale <- .SetToDim(v.scale)

  force(x); force(h.aln); force(v.scale);
  force(v.scale.proportional)
  force(height); force(pad); force(min.ratio);
  function(nr, nc, group=NULL) {

    x <- .GroupSetToDim(x, group)
    h.aln <- .GroupSetToDim(h.aln, group)
    v.scale <- .GroupSetToDim(v.scale, group)

    dm <- x
    dm$nr <- nr
    dm$nc <- nc

    ## vertical alignment
    if (is.null(height)) {
      if (is.null(v.scale)) {
        v.scale <- x
        v.scale.proportional <- TRUE
      } else {
        dm$row.split <- v.scale$row.split
      }
      if (v.scale.proportional) {
        .height <- 1 / v.scale$nr * nr
        .height <- max(min.ratio, .height)
        .height <- min(1/min.ratio, .height)
        dm$height <- .height * .DimDrawHeight(v.scale)
      } else {
        dm$height <- v.scale$height
      }
    } else {
      dm$height <- height
    }
    dm$bottom <- x$bottom+pad+x$height

    ## horizontal alignment
    if (is.null(h.aln)) h.aln <- x
    dm$left <- h.aln$left
    dm$width <- h.aln$width
    dm$column.split <- h.aln$column.split

    dm
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
#' @param h.aln object for horizontal alignment (when NULL, set to x)
#' @param v.scale object for vertical scaling (when NULL, set to x)
#' @param v.scale.proportional when v.scale is provided, whether to make proportional to data
#' @return a dimension generator beneath x
#' @export
Beneath <- function(x, height=NULL, pad=0.01, min.ratio=0.02, h.aln=NULL, v.scale=NULL, v.scale.proportional=FALSE) {

  x <- .SetToDim(x)
  h.aln <- .SetToDim(h.aln)
  v.scale <- .SetToDim(v.scale)

  force(x); force(h.aln); force(v.scale);
  force(v.scale.proportional)
  force(height); force(pad); force(min.ratio);
  function(nr, nc, group=NULL) {

    x <- .GroupSetToDim(x, group)
    h.aln <- .GroupSetToDim(h.aln, group)
    v.scale <- .GroupSetToDim(v.scale, group)

    dm <- x
    dm$nr <- nr
    dm$nc <- nc

    ## vertical alignment
    if (is.null(height)) {
      if (is.null(v.scale)) {
        v.scale <- x
        v.scale.proportional <- TRUE
      } else {
        dm$row.split <- v.scale$row.split
      }
      if (v.scale.proportional) {
        .height <- 1 / v.scale$nr * nr
        .height <- max(min.ratio, .height)
        .height <- min(1/min.ratio, .height)
        dm$height <- .height * .DimDrawHeight(v.scale)
      } else {
        dm$height <- v.scale$height
      }
    } else {
      dm$height <- height
    }
    dm$bottom <- x$bottom-pad-dm$height

    ## horizontal alignment
    if (is.null(h.aln)) h.aln <- x
    dm$left <- h.aln$left
    dm$width <- h.aln$width
    dm$column.split <- h.aln$column.split

    dm
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
#' @param v.aln object for vertical alignment (when NULL, set to x)
#' @param h.scale object for horizontal scaling (when NULL, set to x)
#' @param h.scale.proportional when h.scale is provided, whether to make proportional to data
#' @return a dimension to the left of x
#' @export
LeftOf <- function(x, width=NULL, pad=0.01, min.ratio=0.02, v.aln=NULL, h.scale=NULL, h.scale.proportional=FALSE) {

  x <- .SetToDim(x)
  v.aln <- .SetToDim(v.aln)
  h.scale <- .SetToDim(h.scale)

  force(x); force(v.aln); force(h.scale);
  force(h.scale.proportional)
  force(width); force(pad); force(min.ratio);
  function(nr, nc, group=NULL) {

    x <- .GroupSetToDim(x, group)
    v.aln <- .GroupSetToDim(v.aln, group)
    h.scale <- .GroupSetToDim(h.scale, group)

    dm <- x
    dm$nr <- nr
    dm$nc <- nc

    ## horizontal alignment
    if (is.null(width)) {
      if (is.null(h.scale)) {
        h.scale <- x
        h.scale.proportional <- TRUE
      } else {
        dm$column.split <- h.scale$column.split
      }
      if (h.scale.proportional) {
        .width <- 1 / h.scale$nc * nc
        .width <- max(min.ratio, .width)
        .width <- min(1/min.ratio, .width)
        dm$width <- .width * .DimDrawWidth(h.scale)
      } else {
        dm$wdith <- h.scale$width
      }
    } else {
      dm$width <- width
    }
    dm$left <- x$left-pad-dm$width

    ## vertical alignment
    if (is.null(v.aln)) v.aln <- x
    dm$bottom <- v.aln$bottom
    dm$height <- v.aln$height
    dm$row.split <- v.aln$row.split

    dm
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
#' @param v.aln object for vertical alignment (when NULL, set to x)
#' @param h.scale object for horizontal scaling (when NULL, set to x)
#' @param h.scale.proportional when h.scale is provided, whether to make proportional to data
#' @return a dimension to the right of x
#' @export
RightOf <- function(x, width=NULL, pad=0.01, min.ratio=0.02, v.aln=NULL, h.scale=NULL, h.scale.proportional=FALSE) {

  x <- .SetToDim(x)
  v.aln <- .SetToDim(v.aln)
  h.scale <- .SetToDim(h.scale)

  force(x); force(v.aln); force(h.scale);
  force(h.scale.proportional)
  force(width); force(pad); force(min.ratio);
  function(nr, nc, group=NULL) {

    x <- .GroupSetToDim(x, group)
    v.aln <- .GroupSetToDim(v.aln, group)
    h.scale <- .GroupSetToDim(h.scale, group)

    dm <- x
    dm$nr <- nr
    dm$nc <- nc

    ## horizontal alignment
    if (is.null(width)) {
      if (is.null(h.scale)) {
        h.scale <- x
        h.scale.proportional <- TRUE
      } else {
        dm$column.split <- h.scale$column.split
      }
      if (h.scale.proportional) {
        .width <- 1 / h.scale$nc * nc
        .width <- max(min.ratio, .width)
        .width <- min(1/min.ratio, .width)
        dm$width <- .width * .DimDrawWidth(h.scale)
      } else {
        dm$width <- h.scale$width
      }
    } else {
      dm$width <- width
    }
    dm$left <- dm$left+pad+x$width

    ## vertical alignment
    if (is.null(v.aln)) v.aln <- x
    dm$bottom <- v.aln$bottom
    dm$height <- v.aln$height
    dm$row.split <- v.aln$row.split

    dm
  }
}
