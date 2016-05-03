#' WPlot
#' 
#' WPlot
#' 
#' @param hm an object of class WHeatmap
#' @return \code{NULL}
#' @method WPlot
#' @S3method WPlot
#' @export
WPlot <- function(x, ...) {
  UseMethod('WPlot', x)
}

#' WPlot
#' 
#' WPlot
#' 
#' @method WPlot
#' @S3method WPlot
WPlot.list <- function(obs, mar=c(0.03,0.03,0.03,0.03)) {
  
  mar.bottom = mar[1]
  mar.left = mar[2]
  mar.top = mar[3]
  mar.right = mar[4]
  
  left <- min(sapply(obs, function(x) x$dim[1]))
  right <- max(sapply(obs, function(x) x$dim[1]+x$dim[3]))
  bottom <- min(sapply(obs, function(x) x$dim[2]))
  top <- max(sapply(obs, function(x) x$dim[2]+x$dim[4]))
  width <- right-left
  height <- top-bottom
  
  ## cat(bottom, '\t', left, '\t', top, '\t', right, '\n')
  
  ## resize margin to accomodate texts/labels
  text.dims <- lapply(obs, CalcTextRanges)
  mar.bottom <- mar.bottom + bottom - min(sapply(text.dims, function(x) x$bottom))
  mar.left <- mar.left + left - min(sapply(text.dims, function(x) x$left))
  mar.top <- mar.top + max(sapply(text.dims, function(x) x$top)) - top
  mar.right <- mar.right + max(sapply(text.dims, function(x) x$right)) - right
  
  ## cat(mar.bottom, '\t', mar.left, '\t', mar.top, '\t', mar.right, '\n')
  
  grid.newpage()
  for(ob in obs) {
    ## scale object
    ob$dim[1] <- mar.left + (ob$dim[1]-left) * (1-mar.left-mar.right) / width
    ob$dim[2] <- mar.bottom + (ob$dim[2]-bottom) * (1-mar.top-mar.bottom) / height
    ob$dim[3] <- ob$dim[3] * (1-mar.left-mar.right) / width
    ob$dim[4] <- ob$dim[4] * (1-mar.top-mar.bottom) / height
    
    ## plot
    wplot(ob)
  }
}


#' Top of
#' 
#' Generate dimension top of another object
#' 
#' @param x an object with dimension
#' @return a dimension on top of x
#' @export
TopOf <- function(x, height, pad=0.01) {
  c(x$dim[1], x$dim[2]+pad+x$dim[4], x$dim[3], height)
}

#' Beneath
#' 
#' Generate dimension beneath another object
#' 
#' @param x an object with dimension
#' @return a dimension beneath x
#' @export
Beneath <- function(x, height, pad=0.01) {
  c(x$dim[1], x$dim[2]-pad-height, x$dim[3], height)
}

#' LeftOf
#' 
#' Generate dimension to the left of another object
#' 
#' @param x an object with dimension
#' @return a dimension to the left of x
#' @export
LeftOf <- function(x, width, pad=0.01) {
  c(x$dim[1]-pad-width, x$dim[2], width, x$dim[4])
}

#' RightOf
#' 
#' Generate dimension to the right of another object
#' 
#' @param x an object with dimension
#' @return a dimension to the right of x
#' @export
RightOf <- function(x, width, pad=0.01) {
  c(x$dim[1]+pad+x$dim[3], x$dim[2], width, x$dim[4])
}


