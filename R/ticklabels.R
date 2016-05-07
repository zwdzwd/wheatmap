.TickLabelResample <- function(labels, ticklabels.n) {
  text.height1 <- as.numeric(convertUnit(stringHeight('a'),'npc'))
  total.height <- as.numeric(unit(1,'npc'))
  n.labels <- length(labels)
  if (!is.null(ticklabels.n))
    n.texts <- ticklabels.n
  else if (total.height*1.2 < text.height1*n.labels) {
    n.texts <- floor(total.height/text.height1*0.4)
  } else {
    n.texts <- n.labels
  }
  sample.inds <- round(seq(1, n.labels, length.out=n.texts))
}

.WPrintXTickLabels <- function(hm, cex=1) {
  labels <- colnames(hm$data)
  nc = ncol(hm$data)
  x.mid <- (seq_len(nc)-0.5)/nc
  if (hm$xticklabel.side == 'b') {
    .text.just = 'right'
    .text.y = - hm$xticklabel.pad
    .text.rot = 90
  } else {
    .text.just = 'left'
    .text.y = 1 + hm$xticklabel.pad
    .text.rot = 90
  }
  sample.inds <- .TickLabelResample(labels, hm$xticklabels.n)
  grid.text(labels[sample.inds],
            x=x.mid[sample.inds], y=unit(.text.y,'npc'), rot=.text.rot,
            just=c(.text.just, 'center'), gp=gpar(fontsize=hm$xticklabel.fontsize*cex))
}

.WPrintYTickLabels <- function(hm, cex=1) {
  labels <- rownames(hm$data)
  nr = nrow(hm$data)
  y.mid <- (rev(seq_len(nr))-0.5)/nr
  if (hm$yticklabel.side == 'l') {
    .text.just = 'right'
    .text.x = - hm$yticklabel.pad
  } else {
    .text.just = 'left'
    .text.x = 1 + hm$yticklabel.pad
  }
  sample.inds <- .TickLabelResample(labels, hm$yticklabels.n)
  grid.text(labels[sample.inds],
            x=unit(.text.x,'npc'), y=y.mid[sample.inds],
            just=c(.text.just,'center'), gp=gpar(fontsize=hm$yticklabel.fontsize*min(cex)))
}