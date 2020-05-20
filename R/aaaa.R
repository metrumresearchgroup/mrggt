### settings for the global knit doc
latex_cache <- new.env(parent = emptyenv())
latex_cache$line.breaks <- TRUE
latex_cache$margin <- c(1, 1)
latex_cache$pagewidth <- list('portrait' = c(8.5, 11.0),
                              'landscape' = c(11.0, 8.5))
latex_cache$papersize <- 'letter'
latex_cache$orient <- 'portrait'

### settings per table- reset every time as_latex is run
tbl_cache <- new.env(parent = emptyenv())
tbl_cache$color <- c()
tbl_cache$font_size <- 0
tbl_cache$tbl_width <- 0

# reset tbl_cache
reset_tbl_cache <- function() {
  tbl_cache$color <- c()
  tbl_cache$font_size <- 0
  tbl_cache$tbl_width <- 0
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

line.breaks <- function(x){
  if(!is.logical(x)){
    stop('line.breaks must be specified as either TRUE/FALSE')
  }
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
  latex_cache$pagewidth <- sizing
  latex_cache$papersize <- x
}

#' Global options to set for mrggt that effect the latex rendering
#' @param ... args passed on to assign function. possible values to assign are:
#' - **pagemargin**: numeric vector in inches with format ```c(left margin, right margin)```; default is ```c(1, 1)```
#' - **line.breaks**: logical; allow line breaks in table; default is ```TRUE``` (recommended)
#' - **papersize**: character; default ```'letter'```; options:
#'   - *half letter*: 5.5 x 8.0 in
#'   - *letter*: 8.5 x 11.0 in
#'   - *legal*: 8.5 x 14.0 in
#'   - *junior legal*: 5.0 x 8.0 in
#'   - *ledger*: 11.0 x 17.0 in
#'
#' @examples
#' # set left & right margins to 3in & 4in
#' # no line breaks
#' # change paper to legal
#'
#' mrggt_options('pagemargin' = c(3, 4),
#'              'line.breaks' = FALSE,
#'              'papersize' = 'legal')
#'
#' @export
mrggt_options <- function(...){
  opts <- list(...)
  avail_set <- c('papersize',
                 'line.breaks',
                 'pagemargin',
                 'orient')

  if(!length(names(opts)[!names(opts) %in% avail_set]) == 0){

    message(paste0('ignoring unknown options specified: ',
                   paste(names(opts)[!names(opts) %in% avail_set],
                         collapse = ', ')))
  }

  options <- names(opts)[names(opts) %in% avail_set]
  purrr::walk(options, ~do.call(.x, list(x = opts[[.x]])))
}

