#' column bind non-overlapping objects
#'
#' column bind non-overlapping objects
#'
#' @param ... plotting objects
#' @param nr number of rows
#' @param nc number of columns
#' @return an object of class WDim
#' @export
WColumnBind <- function(..., name='', nr=NULL, nc=NULL) {
  objs <- lapply(list(...), function(o) {
    if (is.character(o)) GetCanvas(o)
    else o
  })
  names(objs) <- sapply(objs, function(o) o$name)

  dms <- lapply(objs, function(o) o$dm)
  dm <- do.call(.DimGroup, dms)
  if (is.null(nc))
    dm$nc <- sum(sapply(dms, function(.dm) .dm$nc))
  else
    dm$nc <- nc
  if (is.null(nr))
    dm$nr <- max(sapply(dms, function(.dm) .dm$nr))
  else
    dm$nr <- nr

  dm$column.split <- lapply(dms, function(.dm) ToAffine(.dm, dm))
  dm
  g
}

#' row bind non-overlapping objects
#'
#' row bind non-overlapping objects
#'
#' @param ... plotting objects
#' @param nr number of rows
#' @param nc number of columns
#' @return an object of class WDim
#' @export
WRowBind <- function(..., name='', nr=NULL, nc=NULL) {
  objs <- lapply(list(...), function(o) {
    if (is.character(o)) GetCanvas(o)
    else o
  })
  names(objs) <- sapply(objs, function(o) o$name)

  dms <- lapply(objs, function(o) o$dm)
  dm <- do.call(.DimGroup, dms)
  if (is.null(nc))
    dm$nc <- max(sapply(dms, function(.dm) .dm$nc))
  else
    dm$nc <- nc
  if (is.null(nr))
    dm$nr <- sum(sapply(dms, function(.dm) .dm$nr))
  else
    dm$nr <- nr

  dm$row.split <- lapply(dms, function(.dm) ToAffine(.dm, dm))
  dm
}
