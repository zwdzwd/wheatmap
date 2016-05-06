
#' @export
`+.WGroup` <- function(group, p) {

  ## first plotting object
  if (!('WGroup' %in% class(group))) {
    group <- WGroup(group, is.root=TRUE)
  }
  group <- .add.WGroup(group, p)
  group
}

#' @export
`+.WHeatmap` <- `+.WGroup`
