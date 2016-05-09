#' @export
WCustomize <- function(mar.left=NULL, mar.right=NULL, mar.top=NULL, mar.bottom=NULL) {
  wc <- list()
  invisible(lapply(names(as.list(match.call()))[-1], function (nm) {
    wc[[nm]] <<- get(nm)
  }))
  force(wc)
  structure(function(group) {
    if (!is.null(wc$mar.left))
      group$mar$left <- wc$mar.left
    if (!is.null(wc$mar.right))
      group$mar$right <- wc$mar.right
    if (!is.null(wc$mar.top))
      group$mar$top <- wc$mar.top
    if (!is.null(wc$mar.bottom))
      group$mar$bottom <- wc$mar.bottom
    group
  }, class = 'WCustomize')
}
