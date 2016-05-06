
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
#' @return an object of class WGroup
#' @export
WGroup <- function(..., name='', parent=NULL,
                   nr=NULL, nc=NULL) {
## row and column.split must be a set separately ??
  objs <- lapply(list(...), function(o) {
    if (is.character(o)) GetCanvas(o)
    else o
  })
  names(objs) <- sapply(objs, function(o) o$name)

  dms <- lapply(objs, function(o) o$dm)
  dm <- do.call(.DimGroup, dms)
  if (is.null(nc))
    dm$nc <- max(sapply(objs, function(o) o$dm$nc))
  else
    dm$nc <- nc
  if (is.null(nr))
    dm$nr <- max(sapply(objs, function(o) o$dm$nr))
  else
    dm$nr <- nr

  ## register group itself
  group.obj <- structure(list(
    parent=parent,
    children=names(objs),
    name=name,
    dm=dm), class='WGroup')
  group.obj <- RegisterCanvas(group.obj)

  ## put childrens dimension to npc of the parent
  lapply(objs, function(obj) {
    obj$dm <- ToAffine(obj$dm, dm)
    obj$parent <- group.obj$name
    RegisterCanvas(obj)
  })

  group.obj
}

CalcTextBounding.WGroup <- function(group.obj) {
  group.dmb <- DimInPoints(group.obj$dm)
  dmb <- do.call(.DimGroup, lapply(group.obj$children, function(nm) {
    obj <- GetCanvas(nm)
    CalcTextBounding(obj)
  }))
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
  lapply(group.obj$children, function(nm) {
    obj <- GetCanvas(nm)
    obj$dm <- ToAffine(FromAffine(obj$dm, group.obj$dm), dm)
    RegisterCanvas(obj)
  })

  new.obj$dm <- ToAffine(new.obj$dm, dm)
  new.obj$parent <- group.obj$name
  new.obj <- RegisterCanvas(new.obj)

  group.obj$dm <- dm
  group.obj$children <- c(group.obj$children, new.obj$name)
  group.obj <- RegisterCanvas(group.obj)

  group.obj
}

#' subset WGroup
#'
#' subset WGroup
#'
#' @param i integer indexing element
#' @export
`[.WGroup` <- function(x, i) {
  if (is.numeric(i))
    GetCanvas(x$children[i])
  else
    GetCanvas(i)
}

# WFlatten <- function(.obs) {
#   obs <- list()
#   for(o in .obs){
#     if ('WGroup' %in% class(o))
#       obs <- c(obs, o$obs)
#     else
#       obs[[length(obs)+1]] <- o
#   }
#   obs
# }

#' show layout
#' @export
ly <- function(x) print(x, layout.only=TRUE)

#' Scale group
#'
#' Scale group to incorporate text on margins
#' @param group.obj group object that needs to be scaled
#' @return scaled group obj (temporary, unregistered on canvas)
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
#   group.obj <- RegisterCanvas(group.obj)
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
print.WGroup <- function(group, mar=c(0.03,0.03,0.03,0.03), stand.alone=TRUE, cex=1, layout.only=FALSE) {

  if (stand.alone) {
    res <- ScaleGroup(group, mar=mar)
    cex <- res$cex
    group <- res$group
    grid.newpage()
  }

  pushViewport(viewport(x=unit(group$dm$left,'npc'), y=unit(group$dm$bottom,'npc'),
                        width=unit(group$dm$width,'npc'), height=unit(group$dm$height,'npc'),
                        just=c('left','bottom')))
  for (child in group$children) {
    ob <- GetCanvas(child)
    plot(ob, stand.alone=FALSE, cex=cex, layout.only=layout.only)
  }
  upViewport()
}

#' @export
plot.WGroup <- print.WGroup

