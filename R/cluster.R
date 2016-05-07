
#' row cluster a matrix
#'
#' row cluster a matrix
#'
#' @param mat input matrix
#' @param hc.method method to use in hclust
#' @return a list of clustered row, column and matrix
#' @export
row.cluster <- function(mat, hc.method='ward.D2') {
  d.row <- dist(mat)
  r <- list()
  r$row.clust <- hclust(d.row)
  r$column.clust <- NULL
  r$mat <- mat[r$row.hc$order,]
  r
}

#' column cluster a matrix
#'
#' column cluster a matrix
#'
#' @param mat input matrix
#' @param hc.method method to use in hclust
#' @return a list of clustered row, column and matrix
#' @export
column.cluster <- function(mat, hc.method='ward.D2') {
  d.column <- dist(t(mat))
  r <- list()
  r$row.clust <- NULL
  r$column.clust <- hclust(d.column)
  r$mat <- mat[,r$column.clust$order]
  r
}

#' row- and column-cluster a matrix
#'
#' row- and column-cluster a matrix
#'
#' @param at input matrix
#' @param hc.method method to use in hclust
#' @return a list of clustered row, column and matrix
#' @import stats
#' @export
both.cluster <- function(mat, hc.method='ward.D2') {
  library(stats)
  d.row <- dist(mat)
  d.column <- dist(t(mat))
  r <- list()
  r$row.clust <- hclust(d.row)
  r$column.clust <- hclust(d.column)
  r$mat <- mat[r$row.clust$order, r$column.clust$order]
  r
}



