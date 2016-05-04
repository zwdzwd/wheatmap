
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
                 sub.dms=NULL, column.split=FALSE, row.split=FALSE) {
  dm <- list(left=left, bottom=bottom, width=width, height=height, nr=nr, nc=nc,
              sub.dms=sub.dms, column.split=column.split, row.split=row.split)
  class(dm) <- 'WDim'
  dm
}

.DimRight <- function(dm) {
  dm$left+dm$width
}

.DimTop <- function(dm) {
  dm$bottom+dm$height
}

.DimGroup <- function(...) {
  dms <- list(...)
  left = min(sapply(dms, function(dm) dm$left))
  bottom = min(sapply(dms, function(dm) dm$bottom))
  width = max(sapply(dms, function(dm) dm$left+dm$width)) - left
  height = max(sapply(dms, function(dm) dm$bottom+dm$height)) - bottom
  WDim(left=left, bottom=bottom, width=width, height=height, sub.dms=dms)
}

#' Class WGroup
#'
#' Class WGroup
#'
#' @param dm dimension
#' @param nr number of rows
#' @param nc number of columns
#' @return an object of class WGroup
#' @export
WGroup <- function(dm=NULL) {
  o <- list(dm=dm)
  o
}

#' group plotting objects
#'
#' group plotting objects
#'
#' @param ... plotting objects
#' @param nr number of rows
#' @param nc number of columns
#' @return an object of class WGroup
#' @export
WBind <- function(..., nr=NULL, nc=NULL) {
  obs <- list(...)
  dm <- do.call(.DimGroup, lapply(obs, function(o)o$dm))
  if (is.null(nc))
    dm$nc <- min(sapply(obs, function(o) o$nc))
  else
    dm$nc <- nc
  if (is.null(nr))
    dm$nr <- min(sapply(obs, function(o) o$nr))
  else
    dm$nr <- nr
  WGroup(dm=dm)
}

#' column group non-overlapping objects
#'
#' column group non-overlapping objects
#'
#' @param ... plotting objects
#' @param nr number of rows
#' @param nc number of columns
#' @return an object of class WGroup
#' @export
WColumnBind <- function(..., nr=NULL, nc=NULL) {
  obs <- list(...)
  dm <- do.call(.DimGroup, lapply(obs, function(o)o$dm))
  if (is.null(nc))
    dm$nc <- min(sapply(obs, function(o) o$nc))
  else
    dm$nc <- nc
  if (is.null(nr))
    dm$nr <- min(sapply(obs, function(o) o$nr))
  else
    dm$nr <- nr
  dm$column.split = TRUE
  WGroup(dm=dm)
}

#' row group non-overlapping objects
#'
#' row group non-overlapping objects
#'
#' @param ... plotting objects
#' @param nr number of rows
#' @param nc number of columns
#' @return an object of class WGroup
#' @export
WRowBind <- function(..., nr=NULL, nc=NULL) {
  obs <- list(...)
  dm <- do.call(.DimGroup, lapply(obs, function(o)o$dm))
  if (is.null(nc))
    dm$nc <- min(sapply(obs, function(o) o$nc))
  else
    dm$nc <- nc
  if (is.null(nr))
    dm$nr <- min(sapply(obs, function(o) o$nr))
  else
    dm$nr <- nr
  dm$row.split = TRUE
  WGroup(dm=dm)
}
