#' WLegend
#' 
#' a legend
#' 
#' @param x WHeatmap object
#' @param orientation horizontal ('h') or vertical ('v') legend
#' @param label.fontsize
#' @param n.stops number of stops in computing continuous legend
#' @return an object of class WLegend
#' @export
WLegend <- function(x, orientation='v', label.fontsize=16, n.stops=8, ...) {
  
  if (orientation=='h') {
    
    if (x$continuous) {
      d <- seq(from=x$cm$dmin, to=x$cm$dmax, length.out=n.stops)
      legend <- WHeatmap(
        matrix(d, nrow=1, dimnames=list(format(d, digits=3))), cmp=CMPar(cm=x$cm),
        xticklabels=TRUE, xticklabel.fontsize=label.fontsize, ...)
    } else {
      legend <- WHeatmap(
        matrix(x$cm$mapper, dimnames=list(NULL, names(x$cm$mapper)), nrow=1), 
        xticklabels=TRUE, continuous=FALSE, xticklabel.fontsize=label.fontsize, ...)
    }
    legend$orientation='h'
    
  } else { # vertical
    
    if (x$continuous) {
      d <- seq(from=x$cm$dmin, to=x$cm$dmax, length.out=n.stops)
      legend <- WHeatmap(
        matrix(d, dimnames=list(format(d, digits=3))), cmp=CMPar(cm=x$cm), 
        yticklabels=TRUE, yticklabel.fontsize=label.fontsize, yticklabel.side='r', ...)
    } else {
      legend <- WHeatmap(
        matrix(x$cm$mapper, dimnames=list(names(x$cm$mapper))), continuous=FALSE,
        yticklabels=TRUE, yticklabel.fontsize=label.fontsize, yticklabel.side='r', ...)
    }
    legend$orientation='v'
  }
  class(legend) <- c(class(legend), 'WLegend')
  legend
}