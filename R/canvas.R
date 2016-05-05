#' @export
ResetCanvas <- function() {
  rm(list=ls(w.canvas), envir=w.canvas)
  w.canvas$naming.index <- 1
  w.canvas$last <- NULL
}

w.canvas <- new.env(parent=emptyenv())

RegisterCanvas <- function(obj) {

  if(is.null(w.canvas$naming.index))
    w.canvas$naming.index <- 1

  if (obj$name=='') {
    obj$name <- paste0('wheatmap.internal.', w.canvas$naming.index)
    w.canvas$naming.index <- w.canvas$naming.index + 1
  }
  if (obj$name %in% names(w.canvas)) {
    message('Object ', obj$name, ' on canvas updated.')
    ## stop()
  }

  message('Register ', class(obj)[1], ': ', obj$name, '.')
  assign(obj$name, obj, envir=w.canvas)
  w.canvas$last <- obj$name
  obj$name
}

GetCanvas <- function(nm) {
  obj <- w.canvas[[nm]]
  if (is.null(obj)) {
    message('Painting object ', nm, 'not found. Abort')
    stop()
  }
  obj
}
