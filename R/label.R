#' construct a WLabel
#' 
#' @export
WLabel <- function(x=NULL, dm=WDim(), name='', fontsize=12, rot=0, color='black') {

  label <- lapply(formals(), eval)
  invisible(lapply(names(as.list(match.call()))[-1], function (nm) {
    label[[nm]] <<- get(nm)
  }))
  class(label) <- c('WLabel', 'WAnnotate', 'WObject')
  force(label)
  structure(function(group) {
    label$dm <- Resolve(label$dm, group)
    label
  }, class=c('WGenerator', 'WObject'))
}

#' print WLabel
#' 
#' @export
print.WLabel <- function(x, cex=1, layout.only=FALSE, stand.alone=TRUE) {

  library(grid)
  if (stand.alone) {
    group <- WGroup(hm)
    print(group)
    return(group)
  }

  if (!layout.only) {
    text.just <- rotate.just(x$dm$text.just, x$rot)
    grid.text(
      x, x=unit(x$dm$text.x,'npc'), y=unit(x$dm$text.y, 'npc'),
      just=text.just, rot=x$rot, gp=gpar(fontsize=x$fontsize, col=x$color))
  }
}

CalcTextBounding.WLabel <- function(label, group) {

  dm <- DimToTop(label, group)
  dm$left <- NPCToPoints(dm$text.x)
  dm$bottom <- NPCToPoints(dm$text.y)
  width <- text.width(label$x, label$fontsize)
  height <- text.height(label$x, label$fontsize)
  if (label$dm$text.just[1]=='right') {
    dm$left <- dm$left - width
    dm$width <- width
  } else if (label$dm$text.just[1]=='center') {
    dm$left <- dm$left - width/2
    dm$width <- width/2
  } else {
    dm$width <- width
  }

  if (label$dm$text.just[1]=='top') {
    dm$bottom <- dm$bottom - height
    dm$height <- height
  } else if (label$dm$text.just[1]=='center') {
    dm$bottom <- dm$bottom - height/2
    dm$height <- height/2
  } else {
    dm$height <- height
  }
  dm
}

