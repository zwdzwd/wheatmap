
#' @param dm absolute coordinate
#' @param dm.sys the affine system
#' @return dm.affine on the affine coordiante
ToAffine <- function(dm, dm.sys) {
  dm.affine <- dm
  dm.affine$left <- (dm$left - dm.sys$left) / dm.sys$width
  dm.affine$bottom <- (dm$bottom - dm.sys$bottom) / dm.sys$height
  dm.affine$width <- dm$width / dm.sys$width
  dm.affine$height <- dm$height / dm.sys$height
  dm.affine
}

#' @param obj object on affine coordinate
#' @param dm.sys the affine system
#' @return object on absolute coordinate
FromAffine <- function(dm.affine, dm.sys) {
  dm <- dm.affine
  dm$left <- dm.sys$left + dm.affine$left * dm.sys$width
  dm$bottom <- dm.sys$bottom + dm.affine$bottom * dm.sys$height
  dm$width <- dm.sys$width * dm.affine$width
  dm$height <- dm.sys$height * dm.affine$height

  dm
}

#' Create a WGroup
#'
#' Children must be registered already
#'
#' @param dm dimension
#' @param nr number of rows
#' @param nc number of columns
#' @param affine member is on affine coordinate
#' @return an object of class WGroup
#' @export
WGroup <- function(..., name='', group.dm=WDim(), affine=FALSE, nr=NULL, nc=NULL) {
  ## row and column.split must be a set separately ??
  objs <- list(...)

  ## convert to affine coordinates if not
  ## the group dm is the merge of the member
  ## dm before affine conversion
  if (!affine) {
    dms <- lapply(objs, function(o) o$dm)
    dm <- do.call(.DimGroup, dms) ## bounding box coordinates before scaling
  
    if (is.null(nc))
      dm$nc <- max(sapply(objs, function(o) o$dm$nc))
    else
      dm$nc <- nc
    if (is.null(nr))
      dm$nr <- max(sapply(objs, function(o) o$dm$nr))
    else
      dm$nr <- nr
  
    objs <- lapply(objs, function(obj) {
      obj$dm <- ToAffine(obj$dm, dm)
      obj
    })
    
    group.dm <- dm
  }

  group.obj <- structure(list(
    children=objs,
    name=name,
    dm=group.dm), class='WGroup')

  ## assign names if missing
  missing.inds <- which(sapply(objs, function(obj) obj$name==''))
  assigned.names <- GroupAssignNames(group.obj, length(missing.inds))
  lapply(seq_along(missing.inds), function(i) {
    group.obj$children[[missing.inds[i]]]$name <<- assigned.names[i]
  })

  stopifnot(GroupCheckNameUnique(group.obj))
  stopifnot(GroupCheckNameValid(group.obj))

  group.obj
}

CalcTextBounding.WGroup <- function(group.obj, top.group=NULL) {
  if (is.null(top.group)) top.group <- group.obj
  group.dmb <- DimInPoints(group.obj$dm)
  dmb <- do.call(
    .DimGroup, lapply(group.obj$children,
                      function(obj) CalcTextBounding(obj, top.group)))
#   dmb <- FromAffine(dmb, group.dmb)
  .DimGroup(dmb, group.dmb)
}

## new.obj should be registered already
AddWGroup <- function(group.obj, new.obj) {
  dm <- .DimGroup(group.obj$dm, new.obj$dm)
  dm$nc <- max(group.obj$dm$nc, new.obj$dm$nc)
  dm$nr <- max(group.obj$dm$nr, new.obj$dm$nr)

  ## put old and new children's dimensions
  ## to npc of the new dimension
  ## olds
  group.obj$children <- lapply(group.obj$children, function(obj) {
    obj$dm <- ToAffine(FromAffine(obj$dm, group.obj$dm), dm)
    obj
  })
  
  if (new.obj$name %in% GroupNames(group.obj)) {
    message('New object name ', new.obj$name, ' conflicts with existing names. Abort.')
    stop()
  }

  if (new.obj$name=='')
    new.obj$name <- GroupAssignNames(group.obj)

  ## new
  new.obj$dm <- ToAffine(new.obj$dm, dm)
  group.obj$dm <- dm
  group.obj$children[[length(group.obj$children)+1]] <- new.obj
  group.obj
}

GroupCheckNameUnique <- function(group.obj) {
  if (!('WGroup' %in% class(group.obj)))
    return(TRUE)
  all.nms <- GroupNames(group.obj)
  if (length(all.nms)!=length(unique(all.nms)))
    return(FALSE)
  for(child in group.obj$children)
    if (!GroupCheckNameUnique(child))
      return(FALSE)
  return(TRUE)
}

GroupCheckNameValid <- function(group.obj) {
  all(GroupAllNames(group.obj)!='')
}

GroupNames <- function(group.obj) {
  sapply(group.obj$children, function(x) x$name)
}

GroupAllNames <- function(group.obj) {
  do.call(c, lapply(group.obj$children, function(x){
    if ('WGroup' %in% class(x))
      GroupAllNames(x)
    else
      x$name
  }))
}

GroupAssignNames <- function(group.obj, n=1) {
  i <- 0
  all.names <- GroupNames(group.obj)
  assigned <- NULL
  repeat{
    i <- i+1
    .name <- paste0('..internal.',i)
    if (!(.name %in% all.names) && n<=1) {
      assigned <- c(assigned, .name)
      n <- n-1
      break
    }
  }
  assigned
}

#' subset WGroup
#'
#' subset WGroup
#'
#' @param i integer indexing element
#' @export
`[.WGroup` <- function(x, i) {
  if(is.numeric(i))
    return(x$children[i])
  for (xx in x$children) {
    if (xx$name == i[1]) {
      if (length(i)>1 && 'WGroup' %in% class(xx))
        return(xx[i[2:length(i)]])
      return(xx)
    }
  }
  return(NULL)
}

GroupDeepGet <- function(x, nm, force.unique=TRUE) {
  objs <- list()
  for (xx in x$children) {
    if (xx$name == nm)
      objs[[length(objs)+1]] <- xx
    if ('WGroup' %in% class(xx))
      objs <- c(objs, GroupDeepGet(xx, nm, force.unique=FALSE))
  }
  if (force.unique) {
    if (length(objs) > 1) {
      message('The name ',nm,' is ambiguous. Please provide full path.')
      stop()
    }
    return(objs[[1]])
  } else {
    return (objs)
  }
}

.GroupNameGet <- function(x, nm) {
  if (length(nm)==1) {
    if (!is.null(x[nm]))
      return(x[nm])
    else
      return(GroupDeepGet(x, nm))
  } else {
    return(x[nm])
  }
}

GroupNameGet <- function(x, nm) {
  obj <- .GroupNameGet(x, nm)
  if (is.null(obj)) {
    message('Object: ',nm,' unfound.')
    stop()
  }
  return(obj)
}

GetParentIn <- function(x, ancestor) {
  if (is.null(x$name)) # is root
    return(NULL)
  if ('WGroup' %in% class(ancestor)) {
    for (child in ancestor$children) {
      if (child$name==x$name)
        return(ancestor)
      if ('WGroup' %in% class(child)) {
        parent <- GetParentIn(x, child)
        if (!is.null(parent))
          return(parent)
      }
    }
  }
  return(NULL)
}

WFlatten <- function(.obs) {
  obs <- list()
  for(o in .obs){
    if ('WGroup' %in% class(o))
      obs <- c(obs, o$obs)
    else
      obs[[length(obs)+1]] <- o
  }
  obs
}

#' show layout
#' @export
ly <- function(x) print(x, layout.only=TRUE)

#' Scale group
#'
#' Scale group to incorporate text on margins
#' @param group.obj group object that needs to be scaled
#' @return scaled group obj
ScaleGroup <- function(group.obj, mar=c(0.03,0.03,0.03,0.03)) {

  mar.bottom = mar[1]
  mar.left = mar[2]
  mar.top = mar[3]
  mar.right = mar[4]

  dmb <- CalcTextBounding(group.obj)
  dmb$left <- dmb$left - dmb$width*mar.left
  dmb$bottom <- dmb$bottom - dmb$height*mar.bottom
  dmb$width <- dmb$width*(1+mar.left+mar.right)
  dmb$height <- dmb$height*(1+mar.bottom+mar.top)
  group.dmb <- DimInPoints(group.obj$dm)
  group.obj$dm <- ToAffine(group.dmb, dmb)
  cex <- c(group.dmb$width / dmb$width,
           group.dmb$height / dmb$height)

  list(cex=cex, group=group.obj)
}

#' Draw WGroup
#'
#' @param group plot to display
#' @param cex for scale fonts
#' @import grid
#' @export
print.WGroup <- function(group, mar=c(0.03,0.03,0.03,0.03),
                         stand.alone=TRUE, cex=1, layout.only=FALSE) {

  if (stand.alone) {
    res <- ScaleGroup(group, mar=mar)
    cex <- res$cex
    group <- res$group
    grid.newpage()
  }

  pushViewport(viewport(x=unit(group$dm$left,'npc'),
                        y=unit(group$dm$bottom,'npc'),
                        width=unit(group$dm$width,'npc'),
                        height=unit(group$dm$height,'npc'),
                        just=c('left','bottom')))
  if (layout.only) {
    grid.rect(gp=gpar(col='green', lwd=5, alpha=0.6))
  }
  for (child in group$children) {
    print(child, stand.alone=FALSE, cex=cex, layout.only=layout.only)
  }
  upViewport()
}

#' @export
plot.WGroup <- print.WGroup

