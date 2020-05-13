latex_cache <- new.env(parent = emptyenv())
latex_cache$shrink <- NULL
latex_cache$line.breaks <- TRUE
latex_cache$column.sep <- NULL
latex_cache$footnotes.align <- 'l'
latex_cache$sourcenotes.align <- 'l'
latex_cache$margin <- c(1, 1)
latex_cache$env <- rlang::caller_env()


# Clear cache for test chart and package info
reset_latex_cache <- function() {
  latex_cache$shrink <- NULL
  latex_cache$line.breaks <- TRUE
  latex_cache$column.sep <- NULL
  latex_cache$footnotes.align <- 'l'
  latex_cache$sourcenotes.align <- 'l'
}

#' Global options to set for mrggt that effect the latex rendering
#' @param ... args passed on to assign function. possible values to assign are:
#' - **margin**: numeric vector with values in inches in format ```c(left margin, right margin)```; default is ```c(1, 1)```
#' - **column.sep**: numeric value in pt; default is 3pt.
#' - **line.breaks**: logical; allow line breaks in table; default is ```TRUE``` (recommended)
#'
#' @examples
#' # set left & right margins to 1in & 2in
#' # no line breaks
#' # change column separation in table to 2pt.
#'
#' mrggtOptions('margin' = c(3, 4),
#'              'line.breaks' = FALSE,
#'              'column.sep' = 2)
#'
#' @export
mrggtOptions <- function(...){
  opts <- list(...)
  assign_multiple <- Vectorize(assign, vectorize.args = c('x', 'value'))
  invisible(assign_multiple(names(opts), opts, envir = latex_cache))
}

