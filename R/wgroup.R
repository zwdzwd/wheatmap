
#' Create a WGroup
#'
#' @param dm dimension
#' @param nr number of rows
#' @param nc number of columns
#' @return an object of class WGroup
#' @export
WGroup <- function(..., name='', nr=NULL, nc=NULL, to.row.split=FALSE, to.column.split=FALSE) {
  obs <- lapply(list(...), function(o) {
    if (is.character(o)) GetCanvas(o)
    else o
  })
  names(obs) <- sapply(obs, function(o) o$name)

  g <- list(obs=obs, name=name)
  dms <- lapply(obs, function(o)o$dm)

  g$dm <- do.call(.DimGroup, dms)
  if (to.row.split)
    g$dm$row.split <- dms
  if (to.column.split)
    g$dm$column.split <- dms

  if (is.null(nc))
    g$dm$nc <- max(sapply(obs, function(o) o$dm$nc))
  else
    g$dm$nc <- nc
  if (is.null(nr))
    g$dm$nr <- max(sapply(obs, function(o) o$dm$nr))
  else
    g$dm$nr <- nr

  class(g) <- 'WGroup'
  g
}

.add.WGroup <- function(group, new.ob) {
  group$obs[[length(group$obs)+1]] <- new.ob
  names(group$obs)[length(group$obs)] <- new.ob$name
  nc <- max(group$dm$nc, new.ob$dm$nc)
  nr <- max(group$dm$nr, new.ob$dm$nr)
  group$dm <- .DimGroup(group$dm, new.ob$dm)
  group$dm$nc <- nc
  group$dm$nr <- nr
  group
}

#' subset WGroup
#'
#' subset WGroup
#'
#' @param i integer indexing element
#' @export
`[.WGroup` <- function(x, i) {
  x$obs[[i]]
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
WGroupColumn <- function(..., name='', nr=NULL, nc=NULL) {
  g <- WGroup(..., nr=nr, nc=nc, name=name, to.column.split=TRUE)
  if (is.null(nc))
    g$dm$nc <- sum(sapply(g$obs, function(o) o$dm$nc))
  g
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
WGroupRow <- function(..., name='', nr=NULL, nc=NULL) {
  g <- WGroup(..., nr=nr, nc=nc, name=name, to.row.split=TRUE)
  if (is.null(nr))
    g$dm$nr <- sum(sapply(g$obs, function(o) o$dm$nr))
  g
}
