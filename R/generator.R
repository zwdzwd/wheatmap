print.WGenerator <- function(xg) {
  x <- xg(NULL)
  group <- WGroup(x)
  print(group)
  return(group)
}
