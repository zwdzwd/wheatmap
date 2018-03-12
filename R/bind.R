#' column bind non-overlapping objects
#'
#' column bind non-overlapping objects
#'
#' @param ... plotting objects
#' @param nr number of rows
#' @param nc number of columns
#' @return an object of class WDim
#' @examples
#' WHeatmap(matrix(rnorm(2000),nrow=40),name='a') +
#'   WHeatmap(matrix(rnorm(30), nrow=3), RightOf(),name='b') +
#'   WColorBarH(1:10, TopOf(WColumnBind('a','b')))
#' @export
WColumnBind <- function(..., nr=NULL, nc=NULL) {

    ## a function returns dm
    objs <- list(...)
    force(nr); force(nc);
    structure(function(group) {
        objs <- lapply(objs, function(o) {
            if (is.character(o)) GroupNameGet(group, o)
            else o
        })
        dms <- lapply(objs, function(o) DimToTop(o, group))
        dm <- do.call(.DimGroup, dms)
        if (is.null(nc))
            dm$nc <- sum(sapply(dms, function(.dm) .dm$nc))
        else
            dm$nc <- nc
        if (is.null(nr))
            dm$nr <- max(sapply(dms, function(.dm) .dm$nr))
        else
            dm$nr <- nr

        dm$column.split <- lapply(dms, function(.dm) ToAffine(.dm, dm))
        WObject(dm=dm)
    }, class=c('WGenerator','WObject'))
}

#' row bind non-overlapping objects
#'
#' row bind non-overlapping objects
#'
#' @param ... plotting objects
#' @param nr number of rows
#' @param nc number of columns
#' @return an object of class WDim
#' @examples
#' WHeatmap(matrix(rnorm(2000),nrow=40),name='a') +
#'   WHeatmap(matrix(rnorm(30), nrow=3), Beneath(),name='b') +
#'   WColorBarV(1:10, LeftOf(WRowBind('a','b')))
#' @export
WRowBind <- function(..., nr=NULL, nc=NULL) {

    ## a function returns dm
    objs <- list(...)
    force(nr); force(nc);
    structure(function(group) {
        objs <- lapply(objs, function(o) {
            if (is.character(o)) GroupNameGet(group, o)
            else o
        })
        dms <- lapply(objs, function(o) DimToTop(o, group))
        dm <- do.call(.DimGroup, dms)
        if (is.null(nc))
            dm$nc <- max(sapply(dms, function(.dm) .dm$nc))
        else
            dm$nc <- nc
        if (is.null(nr))
            dm$nr <- sum(sapply(dms, function(.dm) .dm$nr))
        else
            dm$nr <- nr

        dm$row.split <- lapply(dms, function(.dm) ToAffine(.dm, dm))
        WObject(dm=dm)
    }, class=c('WGenerator','WObject'))
}

