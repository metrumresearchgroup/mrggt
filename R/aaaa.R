latex_cache <- new.env(parent = emptyenv())
latex_cache$shrink <- NULL
latex_cache$line.breaks <- TRUE
latex_cache$column.sep <- NULL
latex_cache$footnotes.align <- 'l'
latex_cache$sourcenotes.align <- 'l'
latex_cache$margin <- c(1, 1)
latex_cache$pagewidth <- list('portrait' = c(8.5, 11.0),
                              'landscape' = c(11.0, 8.5))
latex_cache$papersize <- 'letter'
latex_cache$orient <- 'portrait'
latex_cache$env <- rlang::caller_env()


# Clear cache for test chart and package info
reset_latex_cache <- function() {
  latex_cache$shrink <- NULL
  latex_cache$line.breaks <- TRUE
  latex_cache$column.sep <- NULL
  latex_cache$footnotes.align <- 'l'
  latex_cache$sourcenotes.align <- 'l'
}

orient <- function(x = c('portrait', 'landscape')){
  x <- match.arg(x)
  latex_cache$orient <- x
}

pagemargin <- function(x){
  if(!length(x) == 2){
    stop('margins must be specified as c(left margin, right margin)')
  }
  latex_cache$margin <- x
}

line.breaks <- function(x = c(TRUE, FALSE)){
  x <- match.arg(x)
  latex_cache$line.breaks <- x
}

orient <- function(x = c('portrait', 'landscape')){
  x <- match.arg(x)
  latex_cache$orient <- x
}

papersize <- function(x = c('half letter', 'letter', 'legal', 'junior legal', 'ledger')){
  x <- match.arg(x)
  sizing <- switch(
    x,
    'half letter' = list(
      'portrait' = c(5.5, 8.5),
      'landscape' = c(8.5, 5.5)
    ),
    'letter' = list(
      'portrait' = c(8.5, 11.0),
      'landscape' = c(11.0, 8.5)
    ),
    'legal' = list(
      'portrait' = c(8.5, 14.0),
      'landscape' = c(14.0, 8.5)
    ),
    'junior legal' = list(
      'portrait' = c(5.0, 8.0),
      'landscape' = c(8.0, 5.0)
    ),
    'ledger' = list(
      'portrait' = c(11.0, 17.0),
      'landscape' = c(17.0, 11.0)
    )
  )
  latex_cache$pagewidth <- size
  latex_cache$papersize <- x
}

#' Global options to set for mrggt that effect the latex rendering
#' @param ... args passed on to assign function. possible values to assign are:
#' - **pagemargin**: numeric vector in inches with format ```c(left margin, right margin)```; default is ```c(1, 1)```
#' - **column.sep**: numeric value in pt; default is 3pt.
#' - **line.breaks**: logical; allow line breaks in table; default is ```TRUE``` (recommended)
#' - **papersize**: character; default ```'letter'```; options:
#'   - *half letter*: 5.5 x 8.0 in
#'   - *letter*: 8.5 x 11.0 in
#'   - *legal*: 8.5 x 14.0 in
#'   - *junior legal*: 5.0 x 8.0 in
#'   - *ledger*: 11.0 x 17.0 in
#'
#' @examples
#' # set left & right margins to 1in & 2in
#' # no line breaks
#' # change column separation in table to 2pt.
#' # change paper to legal
#'
#' mrggtOptions('pagemargin' = c(3, 4),
#'              'line.breaks' = FALSE,
#'              'column.sep' = 2,
#'              'papersize' = 'legal')
#'
#' @export
mrggtOptions <- function(...){
  opts <- list(...)
  avail_set <- c('papersize',
                 'line.breaks',
                 'column.sep',
                 'pagemargin')

  if(!length(names(opts)[!names(opts) %in% avail_set]) == 0){

    message(paste0('ignoring unknown options specified: ',
                   paste(names(opts)[!names(opts) %in% avail_set],
                         collapse = ', ')))
  }

  options <- names(opts)[names(opts) %in% avail_set]
  purrr::walk(options, ~do.call(.x, list(x = opts[[.x]])))
}

