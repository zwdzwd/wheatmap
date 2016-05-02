

jet.stops <- c("#00007F", "#00008C", "#00009A", "#0000A8", "#0000B6", "#0000C4", "#0000D2",
               "#0000DF", "#0000ED", "#0000FB", "#000AFF", "#0018FF", "#0025FF", "#0033FF",
               "#0041FF", "#004EFF", "#005CFF", "#006AFF", "#0078FF", "#0085FF", "#0093FF",
               "#00A1FF", "#00AFFF", "#00BDFF", "#00CBFF", "#00D8FF", "#00E6FF", "#00F4FF",
               "#03FFFB", "#11FFED", "#1EFFDF", "#2CFFD2", "#3AFFC4", "#48FFB6", "#55FFA8",
               "#63FF9A", "#71FF8C", "#7FFF7F", "#8CFF71", "#9AFF63", "#A8FF55", "#B6FF48",
               "#C4FF3A", "#D2FF2C", "#DFFF1E", "#EDFF11", "#FBFF03", "#FFF400", "#FFE600",
               "#FFD800", "#FFCB00", "#FFBD00", "#FFAF00", "#FFA100", "#FF9300", "#FF8500",
               "#FF7800", "#FF6A00", "#FF5C00", "#FF4E00", "#FF4100", "#FF3300", "#FF2500",
               "#FF1800", "#FF0A00", "#FB0000", "#ED0000", "#DF0000", "#D20000", "#C40000",
               "#B60000", "#A80000", "#9A0000", "#8C0000", "#7F0000")

#' map data to color
#' @param data numeric vector
#' @param dmax upper bound of data scale
#' @param dmin lower bound of data scale
#' @param stop.points a vector of hexadecimal color for stop points
#' @return a list of scaler, mapper and colors
#' @export
map.to.continuous.color <- function(data, dmax=NULL, dmin=NULL, cmap=NULL, stop.points=NULL, 
                         brewer.name=NULL, brewer.n=3,
                         colorspace.name='rainbow_hcl', colorspace.n=2) {
  
  if (is.null(stop.points)) {
    if (!is.null(cmap)) {
      stop.points <- get(paste0(cmap,'.stops'))
    } else if (!is.null(brewer.name)) {
      ## use display.brewer.all for the brewer colors
      library(RColorBrewer)
      ## note that brewer.n cannot be >8 typically
      stop.points <- brewer.pal(brewer.n, brewer.name)
    } else {
      library(colorspace)
      ## colorspace.name can be
      ## diverge_hcl, diverge_hsv, terrain_hcl, heat_hcl, sequential_hcl and rainbow_hcl
      ## colorspace.n can be very large
      stop.points <- get(colorspace.name)(colorspace.n)
    }
  }
  
  ## cap data
  if (!is.null(dmax))
    data[data>=dmax] <- dmax
  if (!is.null(dmin))
    data[data<=dmin] <- dmin
  
  .dmax <- max(dmax, data)
  .dmin <- min(dmin, data)
  data <- (data - .dmin) / (.dmax-.dmin)
  
  res <- list()
  res$scaler <- function(x) {(x-.dmin)/(.dmax-.dmin)}
  res$mapper <- colorRamp(stop.points, alpha=TRUE)
  res$colors <- apply(res$mapper(data), 1, 
                      function(x) do.call(rgb, c(as.list(x), maxColorValue=255)))
  res
}

map.to.discrete.color <- function(data, colorspace.name='rainbow_hcl', brewer.name=NULL) {
  alphabet <- as.character(unique(data))
  if (!is.null(brewer.name)) {
    library(RColorBrewer)
    mapped.colors <- brewer.pal(length(alphabet), brewer.name)
  } else {
    library(colorspace)
    mapped.colors <- get(colorspace.name)(length(alphabet))
  }
  
  res <- list()
  res$mapper <- setNames(mapped.colors, alphabet)
  res$colors <- res$mapper[as.character(data)]
  res
}

#' WHeatmap object
#' @export
WHeatmap <- function(
  data=NULL, dim=c(0,0,1,1), name=NULL, continuous=TRUE,

  ## discrete heatmap
  label.to.color = NULL, grey.scale = FALSE, grey.scale.range = c(0.1,0.9),

  ## continous heatmap
  cmap = 'jet', norm = NULL, interpolation = FALSE, 
  
  ## tick label on x-axis
  xticklabels = NULL,
  xticklabel.space = 1,
  xticklabel.side = 'bottom',
  xticklabel.rotat = 90,
  xticklabel.pad = 0.1,
  xticklabel.aln = 'center',

  ## tick label on y-axis
  ytick.poses = NULL,
  yticklabels = NULL,
  yticklabel.space = 1,
  yticklabel.side = 'l',
  yticklabel.pad = 0.1,

  ## color scale min and max
  dmin = NULL, dmax = NULL,

  ## alpha
  alpha = 1,

  ## legend
  legend.title = FALSE, legend.title.fontsize = 8, legend.label.fontsize = 8,
                     
  ## graph parameters
  gp = NULL) {

  hm <- lapply(formals(), eval)
  
  ## graph parameters
  hm$gp <- list()
  hm$gp$col <- 'white'
  lapply(names(gp), function(x) {hm$gp[[x]] <<- gp[[x]]})
  
  invisible(lapply(names(as.list(match.call()))[-1], function (nm) {
    hm[[nm]] <<- get(nm)
  }))
  
  ## map to colors
  if (continuous)
    hm$cm <- map.to.continuous.color(hm$data, cmap=hm$cmap)
  else
    hm$cm <- map.to.discrete.color(hm$data)
  
  class(hm) <- 'WHeatmap'
  hm
}

wplot.WHeatmap <- function(hm) {
  pushViewport(viewport(x=unit(hm$dim[1],'npc'), y=unit(hm$dim[2],'npc'),
                       width=unit(hm$dim[3],'npc'), height=unit(hm$dim[4],'npc'), 
                       just=c('left','bottom'), name=hm$name))
  nc = ncol(hm$data)
  nr = nrow(hm$data)
  x = (seq_len(nc)-1)/nc
  y = (rev(seq_len(nr))-1)/nr
  expand.index <- expand.grid(seq_len(nr), seq_len(nc))
  grid.rect(x[expand.index[[2]]], y[expand.index[[1]]],
            width=unit(1/nc, 'npc'), height=unit(1/nr, 'npc'),
            gp=do.call('gpar', c(list(fill=hm$cm$colors), hm$gp)), just=c('left','bottom'))
  upViewport()
}

wplot <- function(x, ...) {
  UseMethod('wplot', x)
}

WCbar <- function(data, orientation, ...) {
  if (orientation=='h') { # horizontal
    cb = WHeatmap(matrix(data, nrow=1), ...)
    cb$orientation='h'
  } else { # vertical
    cb = WHeatmap(matrix(data), ...)
    cb$orientation='v'
  }
  class(cb) <- c(class(cb), 'WCbar')
  cb
}


row.cluster <- function(mat, hc.method='ward.D2') {
  d.row <- dist(mat)
  r <- list()
  r$row.clust <- hclust(d.row)
  r$column.clust <- NULL
  r$mat <- mat[r$row.hc$order,]
  r
}

column.cluster <- function(mat, hc.method='ward.D2') {
  d.column <- dist(t(mat))
  r <- list()
  r$row.clust <- NULL
  r$column.clust <- hclust(d.column)
  r$mat <- mat[,r$column.clust$order]
  r
}

both.cluster <- function(mat, hc.method='ward.D2') {
  d.row <- dist(mat)
  d.column <- dist(t(mat))
  r <- list()
  r$row.clust <- hclust(d.row)
  r$column.clust <- hclust(d.column)
  r$mat <- mat[r$row.clust$order, r$column.clust$order]
  r
}

WDendrogram <- function(clust, dim=c(0,0,1,1), 
                        facing=c("bottom", "top", "left", "right"), name=NULL) {
  dd <- list(clust=clust, facing=facing, dim=dim)
  class(dd) <- c('WDendrogram')
  dd
}

wplot.WDendrogram <- function(dend) {
  pushViewport(viewport(x=unit(dend$dim[1],'npc'), y=unit(dend$dim[2],'npc'),
                        width=unit(dend$dim[3],'npc'), height=unit(dend$dim[4],'npc'), 
                        just=c('left','bottom'), name=dend$name))
  grid.dendrogram(as.dendrogram(dend$clust), facing=dend$facing)
  upViewport()
}

wplot.list <- function(obs, mar=c(0.01,0.01,0.01,0.01)) {
  
  mar.bottom = mar[1]
  mar.left = mar[2]
  mar.top = mar[3]
  mar.right = mar[4]
  
  grid.newpage()
  left <- min(sapply(obs, function(x) x$dim[1]))
  right <- max(sapply(obs, function(x) x$dim[1]+x$dim[3]))
  bottom <- min(sapply(obs, function(x) x$dim[2]))
  top <- max(sapply(obs, function(x) x$dim[2]+x$dim[4]))
  width <- right-left
  height <- top-bottom
  
  for(ob in obs) {
    ## scale
    ob$dim[1] <- mar.left + (ob$dim[1]-left) * (1-mar.left-mar.right) / width
    ob$dim[2] <- mar.bottom + (ob$dim[2]-bottom) * (1-mar.top-mar.bottom) / height
    ob$dim[3] <- ob$dim[3] * (1-mar.left-mar.right) / width
    ob$dim[4] <- ob$dim[4] * (1-mar.top-mar.bottom) / height
    
    ## plot
    wplot(ob)
  }
}

matrix.data <- cbind(matrix(rnorm(20),nrow=4), 5+matrix(rnorm(8),nrow=4))
row.bar.data <- c(1,2,3,1)
column.bar.data <- c(1:6,6)
dimnames(a) <- list(c('w','x','y','z'), c('a','b','c','d','e','f','g'))

r.both <- both.cluster(matrix.data)
row.bar.data <- row.bar.data[r.both$row.clust$order]
column.bar.data <- column.bar.data[r.both$column.clust$order]

wplot(list(
  WHeatmap(r.both$mat, dim=c(0.1,0.1,0.8,0.8)),
  WCbar(row.bar.data, 'v', dim=c(0.01,0.1,0.08,0.8), continuous=FALSE),
  WCbar(column.bar.data, continuous=FALSE, orientation='h', dim=c(0.1,0.91,0.8,0.08)),
  WDendrogram(r.both$row.clust, facing='right', dim=c(-0.1,0.1,0.1,0.8)),
  WDendrogram(r.both$column.clust, facing='bottom', dim=c(0.1,1.0,0.8,0.1))
))

