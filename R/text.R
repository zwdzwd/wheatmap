#' Calculate Text Bounding
#'
#' Calculate bounding box including texts.
#'
#' W.R.T lower left corner of the view port in the unit of points
#' @param x object
#' @export
CalcTextBounding <- function(x) {
  UseMethod('CalcTextBounding', x)
}

NPCToPoints <- function(npc) {
  as.numeric(convertUnit(unit(npc,'npc'),'points'))
}

#' font width and scale to specified font size
#' @import grid
text.width <- function(txt, fontsize=NULL) {
  library(grid)
  w <- as.numeric(convertUnit(stringWidth(txt),'points'))
  if (!is.null(fontsize)) ## scale to the given font size
    w <- w / get.gpar('fontsize')$fontsize * fontsize
  w
}
