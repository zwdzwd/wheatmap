
#' @export
`+.WGroup` <- function(group, p) {

  ## first plotting object
  if (!('WGroup' %in% class(group))) {
    group <- ResolveDim(group, NULL)
    group <- WGroup(group)
  }
  p <- ResolveDim(p, group)
  group <- AddWGroup(group, p)
  group
}

#' @export
`+.WHeatmap` <- `+.WGroup`
