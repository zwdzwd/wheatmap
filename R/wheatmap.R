
#' WHeatmap object
#'
#' Create a heatmap
#'
#' @param data data matrix
#' @param dim plotting dimension c(left, bottom, width, height)
#' @param continuous whether the data is on continuous scale
#' @param cmp an object of CMPar class
#' @param name name of the plot
#' @export
WHeatmap <- function(

  data=NULL, dim=c(0,0,1,1), name=NULL, continuous=TRUE,
  cmp = CMPar(), # colormapping parameters

  ## titles
  title = NULL, title.fontsize=12, title.pad=0.005, title.side='l',

  ## tick label on x-axis
  xticklabels = NULL,
  xticklabel.side = 'b',
  xticklabel.fontsize = 12,
  xticklabel.rotat = 90,
  xticklabel.pad = 0.005,

  ## tick label on y-axis
  yticklabels = NULL,
  yticklabel.side = 'l',
  yticklabel.fontsize = 12,
  yticklabel.pad = 0.005,

  ## alpha
  alpha = 1,

  ## graph parameters
  gp = NULL) {

  hm <- lapply(formals(), eval)

  ## graph parameters
  hm$gp <- list()
  hm$gp$col <- 'white'
  hm$gp$lty <- 'blank'
  lapply(names(gp), function(x) {hm$gp[[x]] <<- gp[[x]]})

  invisible(lapply(names(as.list(match.call()))[-1], function (nm) {
    hm[[nm]] <<- get(nm)
  }))

  ## allow auto-adjust of dimensions
  hm$nr <- nrow(data)
  hm$nc <- ncol(data)
  if (class(hm$dim)=='function') {
    hm$dim <- hm$dim(hm)
  }

  ## map to colors
  if (continuous)
    hm$cm <- MapToContinuousColors(hm$data, cmp=hm$cmp)
  else
    hm$cm <- MapToDiscreteColors(hm$data, cmp=hm$cmp)

  class(hm) <- 'WHeatmap'
  hm
}

#' Calculate Text Ranges
#'
#' Calculate dimension of object with text
#'
#' @method CalcTextRanges WHeatmap
#' @export
CalcTextRanges.WHeatmap <- function(hm) {
  rg = list()
  ## bottom, left, top, right
  rg$left <- hm$dim[1]
  rg$bottom <- hm$dim[2]
  rg$top <- rg$bottom + hm$dim[4]
  rg$right <- rg$left + hm$dim[3]

  if (!is.null(hm$title)) {
    if (hm$title.side=='l') {
      rg$left <- rg$left - text.width(hm$title, hm$title.fontsize) - hm$title.pad
    } else {
      rg$right <- rg$right + text.width(hm$title, hm$title.fontsize) + hm$title.pad
    }
  }

  if (!is.null(hm$yticklabels)) {
    if (hm$yticklabel.side=='l') {
      rg$left <- rg$left - max(sapply(
        rownames(hm$data), function(t) text.width(t, hm$yticklabel.fontsize))) - hm$yticklabel.pad
    } else {
      rg$right <- rg$right + max(sapply(
        rownames(hm$data), function(t) text.width(t, hm$yticklabel.fontsize))) + hm$yticklabel.pad
    }
  }

  if (!is.null(hm$xticklabels)) {
    if (hm$xticklabel.side=='b') {
      rg$bottom <- rg$bottom - max(sapply(
        rownames(hm$data), function(t) text.width(t, hm$yticklabel.fontsize))) - hm$xticklabel.pad
    } else {
      rg$top <- rg$top + max(sapply(
        rownames(hm$data), function(t) text.width(t, hm$yticklabel.fontsize))) + hm$xticklabel.pad
    }
  }

  rg
}

#' WPlot WHeatmap
#'
#' WPlot WHeatmap
#'
#' @param hm an object of class WHeatmap
#' @return \code{NULL}
#' @import grid
#' @export
WPlot.WHeatmap <- function(hm) {
  pushViewport(viewport(x=unit(hm$dim[1],'npc'), y=unit(hm$dim[2],'npc'),
                       width=unit(hm$dim[3],'npc'), height=unit(hm$dim[4],'npc'),
                       just=c('left','bottom'), name=hm$name))
  library(grid)

  nc = ncol(hm$data)
  nr = nrow(hm$data)
  x = (seq_len(nc)-1)/nc
  y = (rev(seq_len(nr))-1)/nr
  expand.index <- expand.grid(seq_len(nr), seq_len(nc))
  grid.rect(x[expand.index[[2]]], y[expand.index[[1]]],
            width=unit(1/nc, 'npc'), height=unit(1/nr, 'npc'),
            gp=do.call('gpar', c(list(fill=hm$cm$colors), hm$gp)), just=c('left','bottom'))
  upViewport()

  ## x tick labels
  if (!is.null(hm$xticklabels)) {
    labels <- colnames(hm$data)
      x.mid <- (seq_len(nc)-0.5)/nc
    if (hm$xticklabel.side == 'b') {
      .text.just = 'top'
      .text.y = 1
      .text.rot = -90
      .vpy = hm$dim[2] - hm$xticklabel.pad
    } else {
      .text.just = 'bottom'
      .text.y = 0
      .text.rot = 90
      .vpy = hm$dim[2] + hm$dim[4] + hm$xticklabel.pad
    }
    pushViewport(viewport(x=unit(hm$dim[1], 'npc'), y=unit(.vpy, 'npc'),
                          width=unit(hm$dim[3],'npc'), height=max(sapply(labels, stringWidth)), just=c('left', .text.just)))
    grid.text(labels, x=x.mid, y=unit(.text.y,'npc'), just=c('left', 'center'), rot=.text.rot, gp=gpar(fontsize=hm$yticklabel.fontsize))
    upViewport()
  }

  ## y tick labels
  if (!is.null(hm$yticklabels)) {
    labels <- rownames(hm$data)
    y.mid <- (rev(seq_len(nr))-0.5)/nr
    if (hm$yticklabel.side == 'l') {
      .text.just = 'right'
      .text.x = 1
      .vpx = hm$dim[1] - hm$yticklabel.pad
    } else {
      .text.just = 'left'
      .text.x = 0
      .vpx = hm$dim[1] + hm$dim[3] + hm$yticklabel.pad
    }
    pushViewport(viewport(x=unit(.vpx, 'npc'), y=unit(hm$dim[2], 'npc'),
                          width=max(sapply(labels, stringWidth)), height=unit(hm$dim[4],'npc'), just=c(.text.just,'bottom')))
    grid.text(labels, x=unit(.text.x,'npc'), y=y.mid, just=c(.text.just,'center'), gp=gpar(fontsize=hm$yticklabel.fontsize))
    upViewport()
  }

  ## titles
  if (!is.null(hm$title)) {
    if (hm$title.side == 'l') {
      .text.just = 'right'
      .text.x = 1
      .vpx = hm$dim[1] - hm$title.pad
    } else {
      .text.just = 'left'
      .text.x = 0
      .vpx = hm$dim[1] + hm$dim[3] + hm$title.pad
    }
    pushViewport(viewport(x=unit(.vpx,'npc'), y=unit(hm$dim[2],'npc'),
                          width=stringWidth(hm$title), height=unit(hm$dim[4],'npc'), just=c(.text.just,'bottom')))
    grid.text(hm$title, x=unit(.text.x,'npc'), y=unit(0.5,'npc'), just=c(.text.just,'center'), gp=gpar(fontsize=hm$title.fontsize))
    upViewport()
  }
}

#' row cluster a matrix
#'
#' row cluster a matrix
#'
#' @param mat input matrix
#' @param hc.method method to use in hclust
#' @return a list of clustered row, column and matrix
#' @export
row.cluster <- function(mat, hc.method='ward.D2') {
  d.row <- dist(mat)
  r <- list()
  r$row.clust <- hclust(d.row)
  r$column.clust <- NULL
  r$mat <- mat[r$row.hc$order,]
  r
}

#' column cluster a matrix
#'
#' column cluster a matrix
#'
#' @param mat input matrix
#' @param hc.method method to use in hclust
#' @return a list of clustered row, column and matrix
#' @export
column.cluster <- function(mat, hc.method='ward.D2') {
  d.column <- dist(t(mat))
  r <- list()
  r$row.clust <- NULL
  r$column.clust <- hclust(d.column)
  r$mat <- mat[,r$column.clust$order]
  r
}

#' row- and column-cluster a matrix
#'
#' row- and column-cluster a matrix
#'
#' @param at input matrix
#' @param hc.method method to use in hclust
#' @return a list of clustered row, column and matrix
#' @import stats
#' @export
both.cluster <- function(mat, hc.method='ward.D2') {
  library(stats)
  d.row <- dist(mat)
  d.column <- dist(t(mat))
  r <- list()
  r$row.clust <- hclust(d.row)
  r$column.clust <- hclust(d.column)
  r$mat <- mat[r$row.clust$order, r$column.clust$order]
  r
}



