
## WLegendInferDim <- function(m, x, group, dm=NULL,
##                             width=NULL, height=NULL, h.aln=NULL, v.aln=NULL) {

##   nr = nrow(m)
##   nc = ncol(m)
##   ## recover to specified dim
##   if (is.null(dm))
##     dm <- Resolve(dm, nr, nc, group)


##   dm <- WDim(0, 0, x$dm$width/x$dm$nc*nc, x$dm$height/x$dm$nr*nr)

##   dm <- Resolve(dm, nr, nc, group)
##   x <- ResolveToTopDim(x, group)
##   h.aln <- ResolveToTopDim(h.aln, group)
##   v.aln <- ResolveToTopDim(v.aln, group)

##   ## specific to legend, we need to ensure
##   ## the dimensions are not exotic
##   if (nr==1) {
##     if (is.null(height))
##       dm$height <- x$height/x$nr
##     else
##       dm$height <- height
##     if (is.null(width))
##       dm$width <- 5*x$width/x$nc
##     else
##       dm$width <- width
##   }

##   if (nc==1) {
##     if (is.null(width))
##       dm$width <- x$width/x$nc
##     else
##       dm$width <- width
##     if (is.null(height))
##       dm$height <- 5*x$height/x$nr
##     else
##       dm$height <- height
##   }

##   if (!is.null(v.aln)) {
##     dm$bottom <- v.aln$bottom
##     dm$height <- v.aln$height
##   }

##   if (!is.null(h.aln)) {
##     dm$left <- h.aln$left
##     dm$width <- h.aln$width
##   }
##   dm
## }


#' WLegendV
#'
#' a vertical legend
#'
#' @param x a plotting object
#' @param label.fontsize label fontsize
#' @param n.stops number of stops in computing continuous legend
#' @return an object of class WLegend
#' @export
WLegendV <- function(x=NULL, dm=NULL, name='',
                     n.stops=20, n.text=5, label.fontsize=12,
                     width=0.1, height=0.1, ...) {

  kargs <- list(...)
  kargs$dm <- dm
  kargs$name <- name
  force(x); force(kargs);
  force(n.stops); force(n.text); force(label.fontsize);
  structure(function(group) {
    if (is.null(x))
      x <- group$children[[length(group$children)]]$name
    
    x <- Resolve(x, group)
    if (x$continuous) {
      d <- seq(from=x$cm$dmin, to=x$cm$dmax, length.out=n.stops)
      kargs$data <- matrix(
        d, dimnames=list(format(d, digits=2, trim=TRUE)))
    } else {
      d <- x$cm$mapper
      d <- d[order(names(d))]
      kargs$data <- matrix(names(d), dimnames=list(names(d), NULL))
      kargs$continuous <- FALSE
    }

    kargs$cm <- x$cm
    legend <- do.call(WHeatmap, kargs)(group)
    nr <- nrow(kargs$data)
    nc <- ncol(kargs$data)
    legend$dm <- Resolve(dm, group, nr=nr, nc=nc,
                         hard.dm=WDim(0,0,width*nc,height*nr,nr=nr,nc=nc))
    legend$yticklabels <- TRUE
    if (x$continuous)
      legend$yticklabels.n <- n.text
    else
      legend$yticklabels.n <- nr
    legend$yticklabel.fontsize <- label.fontsize
    legend$yticklabel.side <- 'r'
    legend$orientation <- 'v'
    class(legend) <- c('WLegendV', class(legend))
    legend
  }, class='WGenerator')
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
WLegendH <- function(x=NULL, dm=NULL, name='',
                     n.stops=20, n.text=5, label.fontsize=12,
                     width=0.1, height=0.1, ...) {

  kargs <- list(...)
  kargs$dm <- dm
  kargs$name <- name
  
  force(x); force(kargs);
  force(n.stops); force(n.text); force(label.fontsize);
  structure(function(group) {
    if (is.null(x))
      x <- group$children[[length(group$children)]]$name

    x <- Resolve(x, group)
    if (x$continuous) {
      d <- seq(from=x$cm$dmin, to=x$cm$dmax, length.out=n.stops)
      kargs$data <- matrix(
        d, nrow=1, dimnames=list(format(d, digits=2, trim=TRUE)))
    } else {
      d <- x$cm$mapper
      d <- d[order(names(d))]
      kargs$data <- matrix(names(d), dimnames=list(NULL, names(d)), nrow=1)
      kargs$continuous <- FALSE
    }

    kargs$cm <- x$cm
    legend <- do.call(WHeatmap, kargs)(group)
    nr <- nrow(kargs$data)
    nc <- ncol(kargs$data)
    ## when dm is from TopOf etc use nr and nc
    ## when dm is from TopLeftOf etc use hard.dm
    legend$dm <- Resolve(dm, group, nr=nr, nc=nc,
                         hard.dm=WDim(0,0,width*nc,height*nr, nr=nr, nc=nc))
    legend$xticklabels <- TRUE
    if (x$continuous)
      legend$xticklabels.n <- n.text
    else
      legend$xticklabels.n <- nc
    legend$xticklabel.fontsize <- label.fontsize
    legend$xticklabel.side <- 'b'
    legend$orientation <- 'h'
    class(legend) <- c('WLegendH', class(legend))
    legend
  }, class='WGenerator')
}
