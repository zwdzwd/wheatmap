#' @export
ResolveDim <- function(x) {
  UseMethod('ResolveDim', x)
}

LengthToTop <- function(obj, .length) {
  if (is.character(obj)) {
    obj <- GetCanvas(obj)
  }

  if (is.null(obj$parent)) {
    return(.length)
  }
  parent <- GetCanvas(obj$parent)
  .length <- .length * parent$dm$width
  return(LengthToTop(parent, .length))
}

DimToTop <- function(obj, dm=NULL) {

  if (is.character(obj)) {
    obj <- GetCanvas(obj)
  }

  if (is.null(dm)) {
    dm <- obj$dm
  }
  if (is.null(obj$parent)) {
    return(dm)
  }
  parent <- GetCanvas(obj$parent)
  dm <- FromAffine(dm, parent$dm)
  return(DimToTop(parent, dm))
}

DimInPoints <- function(dm) {
  dm.new <- dm
  dm.new$left <- NPCToPoints(dm$left)
  dm.new$bottom <- NPCToPoints(dm$bottom)
  dm.new$width <- NPCToPoints(dm$width)
  dm.new$height <- NPCToPoints(dm$height)
  dm.new
}

#' class WDim
#'
#' class WDim
#'
#' @param left left coordinate
#' @param bottom bottom coordinate
#' @param width width
#' @param height height
#' @param column.split a list of WDim objects for column split
#' @param row.split a list of WDim objects for row split
#' @export
WDim <- function(left=0, bottom=0, width=1, height=1, nr=1, nc=1,
                 column.split=NULL, row.split=NULL) {
  dm <- list(left=left, bottom=bottom, width=width, height=height, nr=nr, nc=nc,
             column.split=column.split, row.split=row.split)
  class(dm) <- 'WDim'
  dm
}

.DimRight <- function(dm) {
  dm$left+dm$width
}

.DimTop <- function(dm) {
  dm$bottom+dm$height
}

## Draw height
##
## Height without inter-subdimension space
.DimDrawHeight <- function(dm) {
  if (is.null(dm$row.split)) return(dm$height)
  else return(sum(sapply(dm$row.split, .DimDrawHeight)))
}

## Draw width
##
## Width without inter-subdimension space
.DimDrawWidth <- function(dm) {
  if (is.null(dm$column.split)) return(dm$width)
  else return(sum(sapply(dm$column.split, .DimDrawWidth)))
}

.DimGroup <- function(...) {
  dms <- list(...)
  left = min(sapply(dms, function(dm) dm$left))
  bottom = min(sapply(dms, function(dm) dm$bottom))
  width = max(sapply(dms, function(dm) dm$left+dm$width)) - left
  height = max(sapply(dms, function(dm) dm$bottom+dm$height)) - bottom
  WDim(left=left, bottom=bottom, width=width, height=height)
}


# .SetToDim <- function(x) {
#   if (is.null(x))
#     return (x)
#   else if (is.character(x)) {
#     x <- GetCanvas(x)
#     return (x$dm)
#   } else
#     return (x$dm)
# }

# .GroupSetToDim <- function(x, group) {
#   if (is.character(x)) {
#     if (is.null(group))
#       message('No group provided. This is a bug.')
#     stopifnot(!is.null(group))
#     .x <- group[x]
#     if (is.null(.x))
#       message('Name not found: ', x, '. Make sure objects are named.')
#     x <- .x$dm
#   }
#   x
# }

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
TopOf <- function(x=NULL, height=NULL, pad=0.01, min.ratio=0.02, h.aln=NULL, v.scale=NULL, v.scale.proportional=FALSE) {

  if (is.null(x)) {
    x <- GetCanvas(w.canvas$last)
  }

  if (!('WDim' %in% class(x)))
    x <- DimToTop(x)
  if (!('WDim' %in% class(h.aln)))
    h.aln <- DimToTop(h.aln)
  if (!('WDim' %in% class(v.scale)))
    v.scale <- DimToTop(v.scale)

  force(x); force(h.aln); force(v.scale);
  force(v.scale.proportional)
  force(height); force(pad); force(min.ratio);
  function(nr, nc) {

    # x <- .GroupSetToDim(x, group)
    # h.aln <- .GroupSetToDim(h.aln, group)
    # v.scale <- .GroupSetToDim(v.scale, group)

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
Beneath <- function(x=NULL, height=NULL, pad=0.01, min.ratio=0.02, h.aln=NULL, v.scale=NULL, v.scale.proportional=FALSE) {

  if (is.null(x))
    x <- GetCanvas(w.canvas$last)

  if (!('WDim' %in% class(x)))
    x <- DimToTop(x)
  if (!('WDim' %in% class(h.aln)))
    h.aln <- DimToTop(h.aln)
  if (!('WDim' %in% class(v.scale)))
    v.scale <- DimToTop(v.scale)

  force(x); force(h.aln); force(v.scale);
  force(v.scale.proportional)
  force(height); force(pad); force(min.ratio);
  function(nr, nc) {

    # x <- .GroupSetToDim(x, group)
    # h.aln <- .GroupSetToDim(h.aln, group)
    # v.scale <- .GroupSetToDim(v.scale, group)

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
LeftOf <- function(x=NULL, width=NULL, pad=0.01, min.ratio=0.02, v.aln=NULL, h.scale=NULL, h.scale.proportional=FALSE) {

  if (is.null(x))
    x <- GetCanvas(w.canvas$last)

  if (!('WDim' %in% class(x)))
    x <- DimToTop(x)
  if (!('WDim' %in% class(v.aln)))
    v.aln <- DimToTop(v.aln)
  if (!('WDim' %in% class(h.scale)))
    h.scale <- DimToTop(h.scale)

  force(x); force(v.aln); force(h.scale);
  force(h.scale.proportional)
  force(width); force(pad); force(min.ratio);
  function(nr, nc) {

    # x <- .GroupSetToDim(x, group)
    # v.aln <- .GroupSetToDim(v.aln, group)
    # h.scale <- .GroupSetToDim(h.scale, group)

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
RightOf <- function(x=NULL, width=NULL, pad=0.01, min.ratio=0.02, v.aln=NULL, h.scale=NULL, h.scale.proportional=FALSE) {

  if (is.null(x))
    x <- GetCanvas(w.canvas$last)

  if (!('WDim' %in% class(x)))
    x <- DimToTop(x)
  if (!('WDim' %in% class(v.aln)))
    v.aln <- DimToTop(v.aln)
  if (!('WDim' %in% class(h.scale)))
    h.scale <- DimToTop(h.scale)

  force(x); force(v.aln); force(h.scale);
  force(h.scale.proportional)
  force(width); force(pad); force(min.ratio);
  function(nr, nc) {

    # x <- .GroupSetToDim(x, group)
    # v.aln <- .GroupSetToDim(v.aln, group)
    # h.scale <- .GroupSetToDim(h.scale, group)

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

