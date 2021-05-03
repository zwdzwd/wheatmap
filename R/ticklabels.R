.TickLabelResample <- function(labels, ticklabels.n) {

    text.height1 <- convertUnit(stringHeight('a'),'npc',valueOnly=TRUE)
    total.height <- 1
    n.labels <- length(labels)
    if (!is.null(ticklabels.n))
        n.texts <- ticklabels.n
    else if (total.height*1.2 < text.height1*n.labels) {
        n.texts <- max(floor(total.height/text.height1*0.4),2)
    } else {
        n.texts <- n.labels
    }
    sample.inds <- round(seq(1, n.labels, length.out=n.texts))
    
}

move_labels = function(x0, n, space = 0.03, max_try = 100000, x_min = 0.02, x_max = 1) {
    nlabels = length(x0)
    x = seq(1, n, length.out=nlabels) / n
    xm = x
    Em = sum(abs(x0-xm))
    E = Em
    
    which_one = sample(1:nlabels, max_try, replace=T)
    direction = sample(0:1, max_try, replace=T)
    lapply(seq_along(which_one), function(i) {
        ii = which_one[i]
        if (direction[i] == 0) { # move left
            if (ii == 1) { d = x[ii] - x_min; }
            else { d = x[ii] - x[ii-1] - space; }
            if (d <= 0) return(NULL);
            d = runif(1, 0, d);
            x1 = x
            x1[ii] = x1[ii] - d;
        } else { # move right
            if (ii == nlabels) { d = x_max - x[ii]; }
            else { d = x[ii+1] - x[ii] - space; }
            if (d <= 0) return(NULL);
            d = runif(1, 0, d);
            x1 = x
            x1[ii] = x1[ii] + d;
        }
        E1 = sum(abs(x0-x1))
        if (E1 < E || E/E1*0.2 > runif(1,0,1)) {
            x <<- x1
            E <<- E1
        }
        if (E < Em) {
            Em <<- E
            xm <<- x
        }
    })
    xm
}

XTickLabelUseData = function(hm, cex=1) {

    nc = ncol(hm$data)
    labels = hm$data[1,]
    labels_rle = rle(labels)
    x0 = (c(0,head(cumsum(labels_rle$length),-1)) + labels_rle$length/2) / length(labels)
    x1 = move_labels(x0, length(labels), space=hm[["xticklabel.space"]])

    if (hm$xticklabel.side == 'b') {
        .text.just = 'right'
        .text.y = - hm$xticklabel.pad
    } else {
        .text.just = 'left'
        .text.y = 1 + hm$xticklabel.pad
    }

    for (i in seq_along(x1)) {
        grid.bezier(
            c(x0[i],(x0[i]+x1[i])/2,(x0[i]+x1[i])/2,x1[i]),
            c(1, 1, .text.y, .text.y), gp=gpar(col=hm$cm$mapper[labels_rle$values[i]]))
    }

    .text.rot = hm$xticklabel.rotat
    grid.text(labels_rle$values, x=x1, y=unit(.text.y,'npc'), rot=.text.rot,
        just=c(.text.just, 'center'),
        gp=gpar(col=hm$cm$mapper[labels_rle$values],
            fontsize=hm$xticklabel.fontsize*cex))
}

.WPrintXTickLabels <- function(hm, labels=NULL, use.data=FALSE, cex=1) {

    if (!is.null(use.data) && use.data) {
        return (XTickLabelUseData(hm, cex=cex))
    }

    if (length(labels)==1 && is.logical(labels)) {
        if (labels) {
            labels <- colnames(hm$data)
        } else {
            labels <- NULL
        }
    }

    if (!is.null(labels)) {
        nc = ncol(hm$data)
        x.mid <- (seq_len(nc)-0.5)/nc
        if (hm$xticklabel.side == 'b') {
            .text.just = 'right'
            .text.y = - hm$xticklabel.pad
        } else {
            .text.just = 'left'
            .text.y = 1 + hm$xticklabel.pad
        }
        .text.rot = hm$xticklabel.rotat
        if (!is.logical(hm$xticklabels))
            sample.inds <- which(labels %in% hm$xticklabels)
        else
            sample.inds <- .TickLabelResample(labels, hm$xticklabels.n)
        grid.text(labels[sample.inds],
                  x=x.mid[sample.inds], y=unit(.text.y,'npc'), rot=.text.rot,
                  just=c(.text.just, 'center'), gp=gpar(fontsize=hm$xticklabel.fontsize*cex))
    }
}

.WPrintYTickLabels <- function(hm, labels=NULL, use.data=FALSE, cex=1) {
    ## if (use.data) {
    ##         return (XTickLabelUseData(hm, cex=cex))
    ##     }
    if (length(labels)==1 && is.logical(labels)) {
        if (labels) {
            labels <- rownames(hm$data)
        } else {
            labels <- NULL
        }
    }

    if (!is.null(labels)) {
        nr = nrow(hm$data)
        y.mid <- (rev(seq_len(nr))-0.5)/nr
        if (hm$yticklabel.side == 'l') {
            .text.just = 'right'
            .text.x = - hm$yticklabel.pad
        } else {
            .text.just = 'left'
            .text.x = 1 + hm$yticklabel.pad
        }
        .text.rot = hm$yticklabel.rotat
        
        if (!is.logical(hm$yticklabels))
            sample.inds <- which(labels %in% hm$yticklabels)
        else
            sample.inds <- .TickLabelResample(labels, hm$yticklabels.n)
        
        grid.text(
            labels[sample.inds],
            x=unit(.text.x,'npc'), y=y.mid[sample.inds], rot=.text.rot,
            just=c(.text.just,'center'), gp=gpar(fontsize=hm$yticklabel.fontsize*min(cex)))
    }
}
