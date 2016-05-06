#' @export
ResetCanvas <- function() {
  if (!is.null(w.canvas$verbose))
    verbose.old <- w.canvas$verbose
  else
    verbose.old <- FALSE
  rm(list=ls(w.canvas), envir=w.canvas)
  w.canvas$naming.index <- 1
  w.canvas$last <- NULL
  w.canvas$verbose <- verbose.old
}

#' @export
OptionCanvas <- function(verbose=FALSE) {
  w.canvas$verbose <- verbose
}

w.canvas <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  ## path <- system.file("extdata", package=pkgname, lib.loc=libname)
  ## files <- dir(path)
  ##for(i in seq_len(length(files))){
  ##assign(objname, db, envir=ns)
  ##namespaceExport(ns, objname)
  ResetCanvas()
}

RegisterCanvas <- function(obj) {

  if(is.null(w.canvas$naming.index))
    w.canvas$naming.index <- 1

  if (obj$name=='') {
    obj$name <- paste0('wheatmap.internal.', w.canvas$naming.index)
    w.canvas$naming.index <- w.canvas$naming.index + 1
  }
  if (obj$name %in% names(w.canvas)) {
    if (w.canvas$verbose)
      message('Object ', obj$name, ' on canvas updated.')
    ## stop()
  }

  if (w.canvas$verbose)
    message('Registered ', class(obj)[1], ': ', obj$name, '.')

  assign(obj$name, obj, envir=w.canvas)
  w.canvas$last <- obj$name
  obj
}

GetCanvas <- function(nm) {
  if (length(nm)>1) {
    objs <- lapply(nm, GetCanvas)
    names(objs) <- nm
    return(objs)
  }
  obj <- w.canvas[[nm]]
  if (is.null(obj)) {
    message('Painting object ', nm, 'not found. Abort')
    stop()
  }
  obj
}
