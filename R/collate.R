## collate does not create a group object but can be used for determine coordinates
WCollate <- function()
  if (to.row.split)
    dm$row.split <- dms
if (to.column.split)
  dm$column.split <- dms
to.row.split=FALSE, to.column.split=FALSE


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
