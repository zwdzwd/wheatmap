
#' @export
`+.WGroup` <- function(group, p) {

  ## first plotting object
  if (!('WGroup' %in% class(group))) {
    ResetCanvas()
    group <- WGroup(group)
  }
  group <- .add.WGroup(group, p)
}

#' @export
`+.WHeatmap` <- `+.WGroup`
