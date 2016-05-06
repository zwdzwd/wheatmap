
.WLegend <- function(m, x, group, dm=NULL, name='', width=NULL, height=NULL, h.aln=NULL, v.aln=NULL, kargs=list()) {

  nr = nrow(m)
  nc = ncol(m)

  if (is.null(dm))
    dm <- WDim(0, 0, x$dm$width/x$dm$nc*nc, x$dm$height/x$dm$nr*nr)

  if ('function' %in% class(dm))
    dm <- dm(NULL, nr, nc, group)

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
    dm$bottom <- v.aln$bottom
    dm$height <- v.aln$height
  }

  if (!is.null(h.aln)) {
    dm$left <- h.aln$left
    dm$width <- h.aln$width
  }

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

  kargs <- list(...)
  force(x); force(dm); force(name); force(n.stops);
  force(n.text); force(label.fontsize); force(kargs);
  structure(list(generator=function(group) {
    if (x$continuous) {
      d <- seq(from=x$cm$dmin, to=x$cm$dmax, length.out=n.stops)
      m <- matrix(d, dimnames=list(format(d, digits=2, trim=TRUE)))
    } else {
      d <- x$cm$mapper
      d <- d[order(names(d))]
      m <- matrix(d, dimnames=list(names(d), NULL))
    }

    legend <- .WLegend(m, x, group, dm, name=name, kargs=kargs)
    legend$yticklabels <- TRUE
    if (x$continuous)
      legend$yticklabels.n <- n.text
    legend$yticklabel.fontsize <- label.fontsize
    legend$yticklabel.side <- 'r'
    legend$orientation <- 'v'
    class(legend) <- c('.WLegendV', class(legend))
    legend
  }, class='WLegendV'))
}

ResolveDim.WLegendV <- function(legend, group) {
  legend <- legend$generator(group)
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

  kargs <- list(...)
  force(x); force(dm); force(name); force(n.stops);
  force(n.text); force(label.fontsize); force(kargs);
  ## function that returns the legend
  structure(list(generator=function(group) {
    if (x$continuous) {
      d <- seq(from=x$cm$dmin, to=x$cm$dmax, length.out=n.stops)
      m <- matrix(d, nrow=1, dimnames=list(format(d, digits=2, trim=TRUE)))
    } else {
      d <- x$cm$mapper
      d <- d[order(names(d))]
      m <- matrix(d, dimnames=list(NULL, names(d)), nrow=1)
    }

    legend <- .WLegend(m, x, group, dm, name=name, kargs=kargs)
    legend$xticklabels <- TRUE
    if (x$continuous)
      legend$xticklabels.n <- n.text
    legend$xticklabel.fontsize <- label.fontsize
    legend$xticklabel.side <- 'b'
    legend$orientation <- 'h'
    class(legend) <- c('.WLegendH', class(legend))
    legend
  }, class='WLegendH'))
}

ResolveDim.WLegendH <- function(legend, group) {
  legend <- legend$generator(group)
  legend
}
