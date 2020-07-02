#' @noRd
sanitize_tex_str <- function(x){
  x %>%
    #tidy_gsub("\\\\", "\\\\textbackslash ") %>%
    # remove {} from below
    tidy_gsub("([&%$#_])", "\\\\\\1") %>%
    tidy_gsub("~", "\\\\textasciitilde ") %>%
    tidy_gsub(">", "\\$>\\$") %>%
    tidy_gsub("<", "\\$<\\$") %>%
    tidy_gsub("\\^", "\\\\textasciicircum ") %>%
    tidy_gsub("\u00B1", "\\$\\\\pm\\$")
}

#' @noRd
sanitize_tex_math <- function(x) {
  x %>%
    tidy_gsub("(\\\\+%)", "\\\\%") %>%
    tidy_gsub('\\$\\$', '\\$') %>%
    tidy_gsub("(\\\\_)", '\\_') %>%
    tidy_gsub("(\\\\+\\{)", '\\{') %>%
    tidy_gsub("(\\\\+\\})", '\\}') %>%
    tidy_gsub("\\|", '\\\\vert') %>%
    tidy_gsub('\u00B1', '\\\\pm')
}

#' @noRd
fmt_latex_math <- function (x, ...) {
  UseMethod("fmt_latex_math", x)
}


#' @noRd
fmt_latex_math.default <- function(x){
  math_present <- grepl('\\$\\$.*\\$\\$', x)
  x[math_present] <- sanitize_tex_math(x[math_present])
  x[!math_present] <- sanitize_tex_str(x[!math_present])
  x
}

#' @noRd
fmt_latex_math.data.frame <- function(x){
  x[] <- lapply(x,fmt_latex_math)
  x
}

#' @noRd
fmt_latex_math.matrix <- function(x) apply(x, c(2), fmt_latex_math)
