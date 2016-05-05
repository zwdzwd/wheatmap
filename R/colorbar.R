#' WColorBarV
#'
#' a vertical color bar
#'
#' @param data numeric vector
#' @return an object of class WColorBarV
#' @export
WColorBarV <- function(data, ...) {
  cb = WHeatmap(matrix(data), sub.name='WColorBarV', ...)
  cb$orientation <- 'v'
  cb$cmp$cmap <- NULL
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
  cb = WHeatmap(matrix(data, nrow=1), sub.name='WColorBarH', ...)
  cb$orientation <- 'h'
  cb$cmp$cmap <- NULL
  cb
}


