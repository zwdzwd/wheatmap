
#' @export
`+.WGroup` <- function(group, p) {

  ## first plotting object
  if (!('WGroup' %in% class(group))) {
    group <- Resolve(group, NULL)
    group <- WGroup(group)
  }

  p <- Resolve(p, group)
  group <- AddWGroup(group, p)
  group
}

#' @export
`+.WHeatmap` <- `+.WGroup`

#' @export
`+.WGenerator` <- `+.WGroup`
