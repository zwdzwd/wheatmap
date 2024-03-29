#' WColorBarV
#'
#' a vertical color bar
#'
#' @param data numeric vector
#' @param ... additional options to WHeatmap
#' @param label colorbar label
#' @param label.side t (for top) or b (for bottom)
#' @param label.fontsize label font size
#' @param label.space when label.use.data, the space between labels
#' @param label.use.data use data to show legend in situ
#' @param label.pad label padding
#' @return an object of class WColorBarV
#' @examples
#' WColorBarV(matrix(50:1))
#' @export
WColorBarV <- function(
    data, ...,
    label=NULL, label.side='t',
    label.fontsize = 12, label.pad = 0.005,
    label.space=0.05, label.use.data = FALSE) {

    cb.data <- matrix(data)
    if (!is.null(label)) {
        colnames(cb.data) <- label
        xticklabels <- TRUE
    } else {
        xticklabels <- FALSE
    }
    heat.gen <- WHeatmap(
        cb.data, sub.name='WColorBarV', xticklabels=xticklabels,
        xticklabel.side=label.side, xticklabel.fontsize=label.fontsize,
        yticklabel.pad=label.pad, yticklabel.use.data=label.use.data,
        yticklabel.space = label.space, ...)
    structure(function(group) {
        cb <- heat.gen(group)
        cb$orientation <- 'v'
        cb$cmp$cmap <- NULL
        cb
    }, class=c('WGenerator','WObject'))
}

#' WColorBarH
#'
#' a horizontal color bar
#'
#' @param data numeric vector
#' @param ... additional options to WHeatmap
#' @param label colorbar label
#' @param label.side l (for left) or r (for right)
#' @param label.fontsize label font size
#' @param label.pad label padding
#' @param label.space when label.use.data, the space between labels
#' @param label.use.data use data to show legend in situ
#' @return an object of class WColorBarH
#' @examples
#' WColorBarH(matrix(1:50))
#' @export
WColorBarH <- function(
    data, ...,
    label=NULL, label.side='r',
    label.fontsize = 12, label.pad = 0.005,
    label.space=0.05, label.use.data = FALSE) {
    
    cb.data <- matrix(data, nrow=1)
    if (!is.null(label)) {
        rownames(cb.data) <- label
        yticklabels <- TRUE
    } else {
        yticklabels <- FALSE
    }
    heat.gen <- WHeatmap(
        cb.data, sub.name='WColorBarH', yticklabels=yticklabels,
        yticklabel.side = label.side, yticklabel.fontsize=label.fontsize,
        xticklabel.pad = label.pad, xticklabel.use.data=label.use.data,
        xticklabel.space = label.space, ...)
    
    structure(function(group) {
        cb <- heat.gen(group)
        cb$orientation <- 'h'
        cb$cmp$cmap <- NULL
        cb
    }, class=c('WGenerator','WObject'))
}


