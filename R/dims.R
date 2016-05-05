
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
WDim <- function(left, bottom, width, height, nr=1, nc=1,
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
