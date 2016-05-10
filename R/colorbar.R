#' WColorBarV
#'
#' a vertical color bar
#'
#' @param data numeric vector
#' @return an object of class WColorBarV
#' @export
WColorBarV <- function(data, ...) {
  heat.gen <- WHeatmap(matrix(data), sub.name='WColorBarV', ...)
  structure(function(group) {
    cb <- heat.gen(group)
    cb$orientation <- 'v'
    cb$cmp$cmap <- NULL
    cb
  }, class=c('WGenerator','WObject'))
}

#' WColorBarH
#'
#' a horizontal color bar
#'
#' @param data numeric vector
#' @return an object of class WColorBarH
#' @export
WColorBarH <- function(data, ...) {
  heat.gen <- WHeatmap(matrix(data, nrow=1), sub.name='WColorBarH', ...)
  structure(function(group) {
    cb <- heat.gen(group)
    cb$orientation <- 'h'
    cb$cmp$cmap <- NULL
    cb
  }, class=c('WGenerator','WObject'))
}


