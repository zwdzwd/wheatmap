print.WGenerator <- function(xg) {
  if (stand.alone) {
    x <- xg(NULL)
    group <- WGroup(x)
    print(group)
    return(group)
  }
}
