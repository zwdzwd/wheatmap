#' WHeatmap object
#'
#' Create a heatmap
#'
#' @param data data matrix
#' @param dm plotting dimension c(left, bottom, width, height)
#' @param continuous whether the data is on continuous scale
#' @param cmp an object of CMPar class
#' @param name name of the plot
#' @return one or a list of heatmaps (depends on whether dimension is split)
#' @export
WHeatmap <- function(

  data=NULL, dm=WDim(0,0,1,1), name='', continuous=NULL,
  cmp = CMPar(), # colormapping parameters

  ## titles
  title = NULL, title.fontsize=12, title.pad=0.005, title.side='l',

  ## tick label on x-axis
  xticklabels = NULL,
  xticklabels.n = NULL,
  xticklabel.side = 'b',
  xticklabel.fontsize = 12,
  xticklabel.rotat = 90,
  xticklabel.pad = 0.005,

  ## tick label on y-axis
  yticklabels = NULL,
  yticklabels.n = NULL,
  yticklabel.side = 'l',
  yticklabel.fontsize = 12,
  yticklabel.pad = 0.005,

  ## alpha
  alpha = 1,

  ## subclass name
  sub.name = NULL,

  ## graph parameters
  gp = NULL) {

  hm <- lapply(formals(), eval)

  ## auto-infer continuous/discrete
  if (is.null(continuous)) {
    if (!is.null(cmp$cm))
      hm$continuous <- cmp$cm$continuous
    else if (is.numeric(data) && length(unique(data)) < 8)
      hm$continuous <- FALSE
    else
      hm$continuous <- TRUE
  }

  ## when dm is not given as a default
  hm$dm <- dm

  ## graph parameters
  hm$gp <- list()
  hm$gp$col <- 'white'
  hm$gp$lty <- 'blank'
  lapply(names(gp), function(x) {hm$gp[[x]] <<- gp[[x]]})

  invisible(lapply(names(as.list(match.call()))[-1], function (nm) {
    hm[[nm]] <<- get(nm)
  }))

  nr <- nrow(hm$data)
  nc <- ncol(hm$data)
  if ('function' %in% class(hm$dm)) {
    dm <- hm$dm(nr, nc)
  } else {
    dm <- hm$dm
    dm$nr <- nr
    dm$nc <- nc
  }

  ## map to colors
  if (hm$continuous)
    cm <- MapToContinuousColors(hm$data, cmp=hm$cmp)
  else
    cm <- MapToDiscreteColors(hm$data, cmp=hm$cmp)

  ## split column if dimension indicates so
  if (!is.null(dm$column.split)) {
    all.nc <- sapply(dm$column.split, function(dm) dm$nc)
    sum.nc <- sum(all.nc)
    nc.data <- ncol(data)
    col.inds <- c(0,round(cumsum(all.nc) * nc.data / sum.nc))
    sub.dms <- dm$column.split[order(sapply(dm$column.split, function(dm) dm$left))]
    k <- lapply(seq_along(sub.dms), function(i) {
      sub.dm <- sub.dms[[i]]
      sub.hm <- hm
      sub.hm$dm <- WDim(sub.dm$left, dm$bottom, sub.dm$width, dm$height)
      sub.hm$data <- data[,(col.inds[i]+1):col.inds[i+1], drop=FALSE]
      sub.hm$cmp$cm <- cm
      if (!is.null(sub.hm$name))
        sub.hm$name <- paste0(sub.hm$name, '.', i)
      do.call(WHeatmap, sub.hm)
    })
    w.group <- do.call(WGroupColumn, k)
    w.group$name <- hm$name
    RegisterCanvas(w.group)
    return(w.group)
  }

  hm$dm <- dm

  hm$cm <- cm
  class(hm) <- 'WHeatmap'
  if (!is.null(sub.name))
    class(hm) <- c(sub.name, class(hm))
  hm$name <- RegisterCanvas(hm)
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
  rg$left <- hm$dm$left
  rg$bottom <- hm$dm$bottom
  rg$top <- rg$bottom + hm$dm$height
  rg$right <- rg$left + hm$dm$width

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

#' plot WHeatmap
#'
#' @param hm an object of class WHeatmap
#' @return \code{NULL}
#' @import grid
#' @export
print.WHeatmap <- function(hm, stand.alone=TRUE, layout.only=FALSE) {
  library(grid)

  if (stand.alone) {
    group <- WGroup(hm)
    print(group)
    return(group)
  }
  pushViewport(viewport(x=unit(hm$dm$left,'npc'), y=unit(hm$dm$bottom,'npc'),
                       width=unit(hm$dm$width,'npc'), height=unit(hm$dm$height,'npc'),
                       just=c('left','bottom')))

  if (layout.only) {
    grid.text(hm$name)
    grid.rect(gp=gpar(col='red'))
    upViewport()
    return
  }

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
      .vpy = hm$dm$bottom - hm$xticklabel.pad
    } else {
      .text.just = 'bottom'
      .text.y = 0
      .text.rot = 90
      .vpy = hm$dm$bottom + hm$dm$height + hm$xticklabel.pad
    }
    pushViewport(viewport(x=unit(hm$dm$left, 'npc'), y=unit(.vpy, 'npc'),
                          width=unit(hm$dm$width,'npc'), height=max(sapply(labels, stringWidth)), just=c('left', .text.just)))
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
      .vpx = hm$dm$left - hm$yticklabel.pad
    } else {
      .text.just = 'left'
      .text.x = 0
      .vpx = hm$dm$left + hm$dm$width + hm$yticklabel.pad
    }
    pushViewport(viewport(x=unit(.vpx, 'npc'), y=unit(hm$dm$bottom, 'npc'),
                          width=max(sapply(labels, stringWidth)), height=unit(hm$dm$height,'npc'), just=c(.text.just,'bottom')))

    text.height1 <- as.numeric(convertUnit(stringHeight('a'),'npc'))
    total.height <- as.numeric(unit(1,'npc'))
    n.labels <- length(labels)
    if (!is.null(hm$yticklabels.n))
      n.texts <- hm$yticklabels.n
    else if (total.height*1.2 < text.height1*n.labels) {
      n.texts <- floor(total.height/text.height1*0.4)
    } else {
      n.texts <- n.labels
    }
    sample.inds <- seq(1, n.labels, length.out=n.texts)
    grid.text(labels[sample.inds], x=unit(.text.x,'npc'), y=y.mid[sample.inds],
              just=c(.text.just,'center'), gp=gpar(fontsize=hm$yticklabel.fontsize))
    upViewport()
  }

  ## titles
  if (!is.null(hm$title)) {
    if (hm$title.side == 'l') {
      .text.just = 'right'
      .text.x = 1
      .vpx = hm$dm$left - hm$title.pad
    } else {
      .text.just = 'left'
      .text.x = 0
      .vpx = hm$dm$left + hm$dm$width + hm$title.pad
    }
    pushViewport(viewport(x=unit(.vpx,'npc'), y=unit(hm$dm$bottom,'npc'),
                          width=stringWidth(hm$title), height=unit(hm$dm$height,'npc'), just=c(.text.just,'bottom')))
    grid.text(hm$title, x=unit(.text.x,'npc'), y=unit(0.5,'npc'), just=c(.text.just,'center'), gp=gpar(fontsize=hm$title.fontsize))
    upViewport()
  }
}

#' plot WHeatmap
#'
#' @param hm heatmap to plot
plot.WHeatmap <- print.WHeatmap

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



