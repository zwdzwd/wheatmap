
#' @export
`+.WGroup` <- function(group, p) {

  if ('WCustomize' %in% class(p)) {
    group <- p(group)
    return (group)
  }
  
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
`+.WCustomize` <- `+.WGroup`

#' @export
`+.WGenerator` <- `+.WGroup`
