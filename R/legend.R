
.WLegend <- function(m, x, dm=NULL, name='', sub.name='.WLegend', width=NULL, height=NULL, h.aln=NULL, v.aln=NULL, ...) {

  nr = nrow(m)
  nc = ncol(m)

  if (is.null(dm))
    dm <- WDim(0, 0, x$dm$width/x$dm$nc*nc, x$dm$height/x$dm$nr*nr)

  if ('function' %in% class(dm))
    dm <- dm(nr, nc)

  ## specific to legend, we need to ensure
  ## the dimensions are not exotic
  if (nr==1) {
    if (is.null(height))
      dm$height <- x$dm$height/x$dm$nr
    else
      dm$height <- height
    if (is.null(width))
      dm$width <- 5*x$dm$width/x$dm$nc
    else
      dm$width <- width
  }

  if (nc==1) {
    if (is.null(width))
      dm$width <- x$dm$width/x$dm$nc
    else
      dm$width <- width
    if (is.null(height))
      dm$height <- 5*x$dm$height/x$dm$nr
    else
      dm$height <- height
  }

  if (!is.null(v.aln)) {
    if (is.character(v.aln))
      v.aln <- GetCanvas(v.aln)
    dm$bottom <- v.aln$bottom
    dm$height <- v.aln$height
  }

  if (!is.null(h.aln)) {
    if (is.character(h.aln))
      h.aln <- GetCanvas(h.aln)
    dm$left <- h.aln$left
    dm$width <- h.aln$width
  }

  if (name == '')
    name <- paste0(x$name, '.legend')

  if (x$continuous) {
    legend <- WHeatmap(m, dm=dm, name=name, sub.name=sub.name, cmp=CMPar(cm=x$cm), ...)
  } else {
    legend <- WHeatmap(m, dm=dm, name=name, sub.name=sub.name, continuous=FALSE, ...)
  }
  legend
}


#' WLegendV
#'
#' a vertical legend
#'
#' @param x a plotting object
#' @param label.fontsize label fontsize
#' @param n.stops number of stops in computing continuous legend
#' @return an object of class WLegend
#' @export
WLegendV <- function(x, dm=NULL, name='', n.stops=20, n.text=5, label.fontsize=16, ...) {

  if (is.null(x))
    x <- GetCanvas(w.canvas$last)

  if(is.character(x))
    x <- GetCanvas(x)

  if (x$continuous) {
    d <- seq(from=x$cm$dmin, to=x$cm$dmax, length.out=n.stops)
    m <- matrix(d, dimnames=list(format(d, digits=2, trim=TRUE)))
  } else {
    d <- x$cm$mapper
    d <- d[order(names(d))]
    m <- matrix(d, dimnames=list(names(d), NULL))
  }

  legend <- .WLegend(m, x, dm, name=name, sub.name='WLegendV', ...)
  legend$yticklabels <- TRUE
  if (x$continuous)
    legend$yticklabels.n <- n.text
  legend$yticklabel.fontsize <- label.fontsize
  legend$yticklabel.side <- 'r'
  legend$orientation <- 'v'
  class(legend) <- c('WLegendV', class(legend))
  legend
}


#' WLegendH
#'
#' a horizontal legend
#'
#' @param x a plotting object
#' @param v.aln vertical alignment
#' @param h.aln horizontal alignment
#' @param label.fontsize label fontsize
#' @param n.stops number of stops in computing continuous legend
#' @return WLegendH
#' @export
WLegendH <- function(x, dm=NULL, name='', n.stops=20, n.text=5, label.fontsize=16, ...) {

  if (is.null(x))
    x <- GetCanvas(w.canvas$last)

  if (is.character(x))
    x <- GetCanvas(x)

  if (x$continuous) {
    d <- seq(from=x$cm$dmin, to=x$cm$dmax, length.out=n.stops)
    m <- matrix(d, nrow=1, dimnames=list(format(d, digits=2, trim=TRUE)))
  } else {
    d <- x$cm$mapper
    d <- d[order(names(d))]
    m <- matrix(d, dimnames=list(NULL, names(d)), nrow=1)
  }

  legend <- .WLegend(m, x, dm, name=name, sub.name='WLegendH', ...)
  legend$xticklabels <- TRUE
  if (x$continuous)
    legend$xticklabels.n <- n.text
  legend$xticklabel.fontsize <- label.fontsize
  legend$xticklabel.side <- 'b'
  legend$orientation <- 'h'
  class(legend) <- c('WLegendH', class(legend))
  legend
}
