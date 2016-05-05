#' Calculate Text Ranges
#'
#' Calculate dimensions accounting for texts.
#'
#' @param x object
#' @examples
#' x <- WHeatmap(matrix(rnorm(16),nrow=4))
#' CalcTextRanges(x)
#' @export
CalcTextRanges <- function(x) {
  UseMethod('CalcTextRanges', x)
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
