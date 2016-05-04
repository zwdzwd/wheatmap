#' WColorBarV
#'
#' a vertical color bar
#'
#' @param data numeric vector
#' @return an object of class WColorBarV
#' @export
WColorBarV <- function(data, ...) {
  cb = WHeatmap(matrix(data), ...)
  cb$orientation <- 'v'
  cb$cmp$cmap <- NULL
  class(cb) <- c(class(cb), 'WCbar')
  cb
}

#' WColorBarH
#'
#' a horizontal color bar
#'
#' @param data numeric vector
#' @return an object of class WColorBarH
#' @export
WColorBarH <- function(data, ...) {
  cb = WHeatmap(matrix(data, nrow=1), ...)
  cb$orientation <- 'h'
  cb$cmp$cmap <- NULL
  class(cb) <- c(class(cb), 'WCbar')
  cb
}


