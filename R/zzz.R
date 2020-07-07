#nocov start
register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}

globalVariables(
  c(
    "x",
    ".",
    "::SUMMARY_KEY",
    "are_groups_present",
    "arrange_dfs",
    "b",
    "blue",
    "boxhead",
    "colname",
    "colnames_start",
    "colnum",
    "colnum_final",
    "column_align",
    "column_label",
    "colors",
    "curr_code",
    "curr_name",
    "data_attr",
    "display_name",
    "footnotes",
    "footnotes_to_list",
    "fs_id",
    "fs_id_coalesced",
    "get_groups_rows",
    "g",
    "green",
    "group_label",
    "grpname",
    "grprow",
    "integrate_summary_lines",
    "locname",
    "locnum",
    "missing_text",
    "n",
    "n_cols",
    "obtain_group_ordering",
    "package",
    "palette",
    "red",
    "row_end",
    "rownum",
    "rownum_i",
    "styles",
    "styles_appended",
    "symbol",
    "text",
    "Var1",
    "base_locale_id",
    "dec_sep",
    "group_sep",
    "groupname",
    "parameter",
    "property",
    "property_value",
    "r",
    "rgba",
    "rowname",
    "scss",
    "selector",
    "style",
    "text_col",
    "time",
    "type",
    "value",
    "var",
    "yiq"
    )
  )

#' **gt** package options
#'
#' @section Package options:
#'
#' **gt** uses the following [options()] to configure behavior:
#' \itemize{
#'   \item `gt.row_group.sep`: a separator between groups for the row group
#'   label.
#' }
#' @name gt-options
NULL

gt_default_options <- list(
  gt.row_group.sep = " - ",
  gt.html_tag_check = TRUE
)

# R 3.5 and earlier have a bug on Windows where if x is latin1 or unknown and
# replacement is UTF-8, the UTF-8 bytes are inserted into the result but the
# result is not marked as UTF-8.
# Example: gsub("a", "\u00B1", "a", fixed = TRUE)
utf8_aware_sub <- NULL

.onLoad <- function(libname, pkgname, ...) {

  register_s3_method("knitr", "knit_print", "gt_tbl")
  register_s3_method("htmltools", "as.tags", "gt_tbl")
  registerS3method("lscape", "gt_tbl", "lscape.gt_tbl")
  #registerS3method("lscape", "list", lscape.list)
  registerS3method("+", "lscape_asis", '+.lscape_asis')
  registerS3method("fmt_latex_math", "default", "fmt_latex_math.default")
  registerS3method("fmt_latex_math", "data.frame ", "fmt_latex_math.data.frame")
  registerS3method("fmt_latex_math", "matrix", "fmt_latex_math.matrix")
  registerS3method("fmt_latex_math", "list", "fmt_latex_math.list")
  registerS3method("preamble", "knit_asis", "preamble.knit_asis")
  registerS3method("save_latex", "knit_asis", "save_latex.knit_asis")
  registerS3method("save_latex", "preamble", "save_latex.preamble")
  registerS3method("find_colors", "default", "find_colors.default")
  registerS3method("find_colors", "list", "find_colors.list")
  registerS3method("find_colors", "cell_fill", "find_colors.cell_fill")
  registerS3method("find_colors", "cell_text", "find_colors.cell_text")


  op <- options()
  toset <- !(names(gt_default_options) %in% names(op))
  if (any(toset)) options(gt_default_options[toset])

  utf8_aware_sub <<- identical("UTF-8", Encoding(sub(".", "\u00B1", ".", fixed = TRUE)))

  invisible()
}

.onAttach <- function(libname, pkgname) {
  setHook(packageEvent("gt", "attach"), function(...) {
    packageStartupMessage(cli::rule())
    packageStartupMessage(
      "You have loaded gt after mrggt - this is likely ",
      "to cause problems.\nIf you need functions from both gt and mrggt, ",
      "please load gt first, then mrggt:\nlibrary(gt); library(mrggt)"
    )
    packageStartupMessage(cli::rule())
  })
}

.onDetach <- function(libpath) {
  setHook(packageEvent("gt", "attach"), NULL, "replace")
}
#nocov end
