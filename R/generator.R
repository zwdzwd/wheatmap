
#' print a WGenerator
#'
#' This calls WGenerator and creates a WGroup to enclose the produced object.
#'
#' @param xg a WGenerator object
#' @return the WGroup containing the plotting object
#' @export
print.WGenerator <- function(xg) {
  x <- xg(NULL)
  group <- WGroup(x)
  print(group)
  return(group)
}
