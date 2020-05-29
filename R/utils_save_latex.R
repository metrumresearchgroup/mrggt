#' Output a **gt knit_asis** or **gt preamble** object to a tex file
#'
#' @usage
#'
#'  ## Default S3 method:
#'  save_latex(tex, path, ...)
#'
#'  ## S3 method for class `knit_asis`:
#'  save_latex(tex, path, preamble = TRUE, append = FALSE)
#'
#'  ## S3 method for class `preamble`:
#'  save_latex(tex, path, append = FALSE)
#'
#'
#'
#' @param tex a `knit_asis` object that is created using the `as_latex()` function or a `preamble` object created using `preamble()` function.
#' @param path path or connection to write to.
#' @param preamble include full preamble with document declaration and document environment wrapping table; default is `FALSE`. Ignored if `tex` is `preamble` object.
#' @param append If `FALSE`, will overwrite existing file. If `TRUE`, will append to existing file. In both cases, if file does not exist a new file is created.
#' @param ... further arguments passed to or used by methods.
#'
#' @examples
#' # Use `gtcars` to create a gt table;
#' # add a header and then export as
#' # tex file with preamble and document declarations
#'
#'   gtcars %>%
#'   dplyr::select(mfr, model) %>%
#'   dplyr::slice(1:2) %>%
#'   gt() %>%
#'   tab_header(
#'     title = md("Data listing from **gtcars**"),
#'     subtitle = md("`gtcars` is an R dataset")
#'   ) %>%
#'   as_latex() %>%
#'   save_latex('tex.tex', preamble = TRUE)
#'
#' # Use `gtcars` to create a gt table;
#' # add a header and then export
#' # just the preamble (package declarations + option settings) to file.
#'
#'   gtcars %>%
#'   dplyr::select(mfr, model) %>%
#'   dplyr::slice(1:2) %>%
#'   gt() %>%
#'   tab_header(
#'     title = md("Data listing from **gtcars**"),
#'     subtitle = md("`gtcars` is an R dataset")
#'   ) %>%
#'   as_latex() %>%
#'   preamble(full_tex = FALSE) %>%
#'   save_latex('preamble.tex')
#'
#' @export
save_latex <- function(tex, path, ...) {
  UseMethod('save_latex')
}

save_latex.knit_asis <-
  function(tex,
           path,
           preamble = FALSE,
           append = FALSE) {
    if (preamble) {

      save_latex(preamble.knit_asis(tex), path, append)

    } else{
      readr::write_file(as.character(tex), path, append)
    }
  }

save_latex.preamble <- function(tex, path, append = FALSE) {
  readr::write_file(tex, path, append)
}
