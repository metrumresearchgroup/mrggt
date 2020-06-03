#' @noRd
gsub_multiple <- function(x, substitues) {
  subs <- purrr::map(substitues, ~ list(
    pattern = .x[1],
    replacement = .x[2]
  ))

  purrr::reduce(subs,
                ~ do.call(gsub,
                          append(.y,
                                 list(x = .x))),
                .init = x)
}

#' @noRd
sanitize_tex <- function (x) {
  UseMethod("sanitize_tex", x)
}

#' @noRd
sanitize_tex.default <- function(x){
  x %>%
  #tidy_gsub("\\\\", "\\\\textbackslash ") %>%
  # remove {} from below
  tidy_gsub("([&%$#_])", "\\\\\\1") %>%
  tidy_gsub("~", "\\\\textasciitilde ") %>%
  tidy_gsub("\\^", "\\\\textasciicircum ")
}
# sanitize_tex.default <- function(x) {
#   sanitize <- list(c("(\\\\+%)", "\\\\%"),
#                    c("CHECKMARK", '\\\\text\\{\\\\checkmark\\}'),
#                    c('>', '\\$>\\$'),
#                    c('<', '\\$<\\$'),
#                    c('\u00B1', '\\$\\\\pm\\$'),
#                    c('(\\_|\\\\+_)', '\\_')
#                    )
#
#   gsub_multiple(x, sanitize)
# }

#' @noRd
sanitize_tex.math <- function(x) {
  vect <- unlist(qdapRegex::rm_between(x, '<', '>', extract = TRUE))
  sanitize <- list(c("(\\\\+%)", "\\\\%"),
                   c('\\$\\$', ''),
                   c("(\\\\_)", '\\_'),
                   c("(\\\\+\\{)", '\\{'),
                   c("(\\\\+\\})", '\\}'),
                   c("CHECKMARK", '\\\\text\\{\\\\checkmark\\}'),
                   c("\\|", '\\\\vert'),
                   c('\u00B1', '\\\\pm'))

  vect <- vect[!is.na(vect)]
  if (length(vect) > 0) {
    sanitize <- append(sanitize,
                       mapply(
                         c,
                         paste0('<', vect, '>'),
                         paste0('\\\\text\\{', vect, '\\}'),
                         SIMPLIFY = F,
                         USE.NAMES = FALSE
                       ))
  }
  gsub_multiple(x, sanitize)
}

#' @noRd
as.tex_math <- function (x) {
  UseMethod("as.tex_math", x)
}

#' @noRd
as.tex_math.default <- function(x) {
  x <- sanitize_tex(x)

  structure(
    list(
      .Label = x,
      math_env = x,
      math_env2exp = rlang::quo(paste0(x))
    ),
    class = c('tex_math')
  )
}

#' @noRd
as.tex_math.math <- function(x){
  x <- sanitize_tex(x)

  structure(
    list(
      .Label = x,
      math_env = paste0('$', x, '$'),
      math_env2exp = rlang::quo(paste0('$', x, '$'))
    ),
    class = c('tex_math')
  )
}

#' @noRd
as.list.tex_math <- function(x){
  list(.Label = x$.Label,
       math_env = x$math_env,
       math_env2exp = x$math_env2exp)
}

extract <- function(x, y){
  UseMethod('extract')
}

extract.list <- function(x, y){
  purrr::map_chr(x, extract, y)
}

extract.data.frame <- function(x, y){
  dfbase <- purrr::map(x, extract, y)
  as.data.frame(dfbase,
                row.names = rownames(x),
                stringsAsFactors = FALSE)
}

#' @noRd
extract.default <- function(x, y){
  purrr::pluck(x, y)
}

#' @noRd
print.tex_math <- function(x){
  print(x$.Label)
}

#' @noRd
c.tex_math <- function(...){
  structure(list(...),
            class = c('tex_math'))
}

#' @noRd
tex_math <- function(x){
  if(grepl('\\$\\$', x)){
    x <- structure(x,
                   class = c(class(x), 'math'))
  }
  x %>% as.tex_math()
}

#' @noRd
fmt_latex_math <- function (x, ...) {
  UseMethod("fmt_latex_math", x)
}


#' @noRd
fmt_latex_math.default <- function(x){
  vtex_math <- Vectorize(tex_math,
                         vectorize.args = c('x'),
                         USE.NAMES = FALSE,
                         SIMPLIFY = FALSE)
  x <- vtex_math(x)
  if(length(x) == 1){
    x <- x[[1]]
  }
  x
}

#' @noRd
fmt_latex_math.data.frame <- function(x){
  dfbase <- rapply(x, fmt_latex_math, how = 'list')
  structure(dfbase,
            row.names = rownames(x),
            class = c('data.frame',
                      'data.frame.tex_math'))
}

#' @noRd
fmt_latex_math.matrix <- function(x) apply(x, c(1, 2), fmt_latex_math)
