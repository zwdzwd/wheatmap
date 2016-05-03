#' WCbar
#' 
#' a color bar
#' 
#' @param data numeric vector
#' @param orientation horizontal ('h') or vertical ('v') color bar
#' @return an object of class WCbar
#' @export
WCbar <- function(data, orientation='h', ...) {
  if (orientation=='h') { # horizontal
    cb = WHeatmap(matrix(data, nrow=1), ...)
    cb$orientation <- 'h'
  } else { # vertical
    cb = WHeatmap(matrix(data), ...)
    cb$orientation <- 'v'
  }
  cb$cmp$cmap <- NULL
  class(cb) <- c(class(cb), 'WCbar')
  cb
}