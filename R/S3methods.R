#' @export
Resolve <- function(x, ...) {
  UseMethod('Resolve')
}

#' Calculate Text Bounding
#'
#' Calculate bounding box including texts.
#'
#' W.R.T lower left corner of the view port in the unit of points
#' @param x object
#' @export
CalcTextBounding <- function(x, ...) {
  UseMethod('CalcTextBounding', x)
}


