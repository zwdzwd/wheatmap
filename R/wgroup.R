
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

