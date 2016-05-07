
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
