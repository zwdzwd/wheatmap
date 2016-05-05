
#' @export
`+.WGroup` <- function(group, p) {

  ## first plotting object
  if (!('WGroup' %in% class(group))) {
    group <- WGroup(group)
  }
  group <- .add.WGroup(group, p)
  group
}

#' @export
`+.WHeatmap` <- `+.WGroup`
