
#' @export
`+.WGroup` <- function(group, p) {

  ## first plotting object
  if (!('WGroup' %in% class(group))) {
    ResetCanvas()
    group <- .GroupSetup(group)

    group <- WGroup(group)
  }
  p <- .GroupSetup(p, group)
  RegisterCanvas(p)
  group <- .add.WGroup(group, p)
}

#' @export
`+.WHeatmap` <- `+.WGroup`

.GroupSetup <- function(x, ...) {
  UseMethod('.GroupSetup', x)
}

.GroupSetup.WDendrogram <- function(dend, group=NULL) {
  ## allow auto-adjust of dimensions
  if (class(dend$dm)=='function')
    dend$dm <- dend$dm(1, 1, group)
  dend
}


.GroupSetup.WHeatmap <- function(hm, group=NULL) {

  nr <- nrow(hm$data)
  nc <- ncol(hm$data)
  if (class(hm$dm)=='function') {
    dm <- hm$dm(nr, nc, group)
  } else {
    dm <- hm$dm
    dm$nr <- nr
    dm$nc <- nc
  }
  ## split column if dimension indicates so
  if (!is.null(dm$column.split)) {
    all.nc <- sapply(dm$column.split, function(dm) dm$nc)
    sum.nc <- sum(all.nc)
    nc.data <- ncol(data)
    col.inds <- c(0,round(cumsum(all.nc) * nc.data / sum.nc))
    sub.dms <- dm$column.split[order(sapply(dm$column.split, function(dm) dm$left))]
    w.group <- do.call(WGroupColumn, lapply(seq_along(sub.dms), function(i) {
      sub.dm <- sub.dms[[i]]
      sub.hm <- hm
      sub.hm$dm <- WDim(sub.dm$left, dm$bottom, sub.dm$width, dm$height)
      sub.hm$data <- data[,(col.inds[i]+1):col.inds[i+1], drop=FALSE]
      sub.hm$cmp$cm <- hm$cm
      sub.hm <- do.call(WHeatmap, sub.hm)
      .GroupSetup(sub.hm)
    }))
    return(w.group)
  }

  hm$dm <- dm
  hm
}

