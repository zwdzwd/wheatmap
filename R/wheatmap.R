
#' WHeatmap object
#' @export
WHeatmap <- function(data,
                     continuous=TRUE,

                     ## discrete heatmap
                     label.to.color = NULL,
                     grey.scale = FALSE,
                     grey.scale.range = (0.1,0.9),

                     ## continous heatmap
                     cmap = 'jet',
                     norm = NULL,
                     interpolation = FALSE,

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
                     dmin = NULL,
                     dmax = NULL,

                     ## alpha
                     alpha = 1,

                     ## legend
                     legend.title = FALSE,
                     legend.title.fontsize = 8,
                     legend.label.fontsize = 8
                     ) {

  hm <- formals()
  lapply(names(as.list(match.call()))[-1], function (nm) {
    hm[[nm]] <<- get(nm)
  })
  class(hm) <- 'WHeatmap'
  hm
}

.plot.WHeatmap.continous <- function(hm, dim) {
  pushViewport(viewport(x=dim[1], y=dim[2], width=dim[3], height=dim[4],
                        name=paste(hm$name, 'heatmap_body', k, sep='_'),...))
  nc = ncol(hm$data)
  nr = nrow(hm$data)
  x = (seq_len(nc)-0.5)/nc
  y = (rev(seq_len(nr))-0.5)/nr
  expand.index <- expand.grid(seq_len(nr), seq_len(nc))
  grid.rect(x[expand.index[[2]]], y[expand.index[[1]]],
            width=unit(1/nc,,'npc'), height=unit(1/nr, 'npc'),
            gp=do.call('gpar', c(list(fill=col.matrix), gp)))
  upViewport()
}

plot.WHeatmap <- function(hm, dim) {

  if (self.continous) {
    .plot.WHeatmap.continous(hm, dim)
  } else {
    .plot.WHeatmap.discrete(hm)
  }
}


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
