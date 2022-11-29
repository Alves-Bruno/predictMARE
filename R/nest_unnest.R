#' Nest a data.table by group
#' 
#' This function was originally written by TS Barrett in a document available at https://osf.io/f6pxw/download.
#' @export
group_nest_dt <- function(dt, ..., .key = "data"){
  stopifnot(is.data.table(dt))
  by <- substitute(list(...))
  dt <- dt[, list(list(.SD)), by = eval(by)]
  setnames(dt, old = "V1", new = .key)
  dt
}

#' Unnest a data.table
#' 
#' This function was originally written by TS Barrett in a document available at https://osf.io/f6pxw/download.
#' @export
unnest_dt <- function(dt, col, id){
  stopifnot(is.data.table(dt))
  by <- substitute(id)
  col <- substitute(unlist(col, recursive = FALSE))
  dt[, eval(col), by = eval(by)]
}
