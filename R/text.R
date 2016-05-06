#' Calculate Text Bounding
#'
#' Calculate bounding box including texts.
#'
#' W.R.T lower left corner of the view port in the unit of points
#' @param x object
#' @examples
#' x <- WHeatmap(matrix(rnorm(16),nrow=4))
#' CalcTextRanges(x)
#' @export
CalcTextBounding <- function(x) {
  UseMethod('CalcTextBounding', x)
}

#' font width and scale to specified font size
#' @import grid
text.width <- function(txt, fontsize=NULL) {
  library(grid)
  w <- as.numeric(convertUnit(stringWidth(txt),'npc'))
  if (!is.null(fontsize))
    w <- w / get.gpar('fontsize')$fontsize * fontsize
  w
}
