# needs to be implemented: \caption{\thelongtablecaption}\\[\bigskipamount]
# Create a vector of LaTeX packages to use as table dependencies
latex_packages <- function() {
  c("amsmath", "booktabs", "caption", "longtable", "xcolor", "amssymb", "color", "colortbl", "array", "mathptmx", "tikz", "pdflscape", "everypage", "threeparttablex")
}

# If the rmarkdown package is available, use the
# `latex_dependency()` function to load latex packages
# without requiring the user to do so
create_knit_meta <- function(shrink = FALSE){
if (requireNamespace("rmarkdown", quietly = TRUE)) {

  latex_packages <-
    lapply(latex_packages(), rmarkdown::latex_dependency)

  if(!shrink){
    latex_packages[[4]]$extra_lines <- c("\\setlength{\\LTleft}{0pt plus 1fill minus 1fill}",
                                         "\\setlength{\\LTright}{\\LTleft}")
  }

  latex_packages[[3]]$options <- c('singlelinecheck=off')
  latex_packages[[11]]$extra_lines <- c('\\def\\checkmark{\\tikz\\fill[scale=0.4](0,.35) -- (.25,0) -- (1,.7) -- (.25,.15) -- cycle;}')
  latex_packages[[13]]$extra_lines <- c('\\newlength{\\hfoot}',
                                        '\\newlength{\\vfoot}',
                                        '\\AddEverypageHook{\\ifdim\\textwidth=\\linewidth\\relax',
                                        '\\else\\setlength{\\hfoot}{-\\topmargin}%',
                                        '\\addtolength{\\hfoot}{-\\headheight}%',
                                        '\\addtolength{\\hfoot}{-\\headsep}%',
                                        '\\addtolength{\\hfoot}{-.5\\linewidth}%',
                                        '\\ifodd\\value{page}\\setlength{\\vfoot}{\\oddsidemargin}%',
                                        '\\else\\setlength{\\vfoot}{\\evensidemargin}\\fi%',
                                        '\\addtolength{\\vfoot}{\\textheight}%',
                                        '\\addtolength{\\vfoot}{\\footskip}%',
                                        '\\raisebox{\\hfoot}[0pt][0pt]{\\rlap{\\hspace{\\vfoot}\\rotatebox[origin=cB]{90}{\\thepage}}}\\fi}')
  latex_packages[[14]]$extra_lines <- c('\\def\\settotextwidth{\\renewcommand\\TPTminimum{\\textwidth}}')

} else {
  latex_packages <- NULL
}
  latex_packages
}

# Transform a footnote mark to a LaTeX representation as a superscript
footnote_mark_to_latex <- function(mark) {

  paste0("\\textsuperscript{", mark, "}")
}

#' @noRd
latex_body_row <- function(content,
                           type) {

  if (type == "row") {

    return(
      paste(paste(content, collapse = " & "), "\\\\ \n"))

  } else if (type == "group") {

    return(
      paste(paste(content, collapse = " & "), "\\\\ \n"))
  }
}

#' @noRd
latex_heading_row <- function(content) {

  paste0(paste(content, collapse = " & "), "\\\\ \n")

}

#' @noRd
latex_group_row <- function(group_name,
                            top_border = TRUE,
                            bottom_border = TRUE,
                            n_cols) {

  multi <- paste0("\\multicolumn{", n_cols, "}{l}{")
  paste0(
    ifelse(top_border, "\\midrule\n", ""),
    multi, group_name,
    "} \\\\ \n",
    ifelse(bottom_border, "\\midrule\n", ""),
    collapse = "")
}


#' @noRd
create_table_start_l <- function(data){
  optimize_width <- dt_options_get_value(data, 'table_optimize_width')
  optimize_font <- dt_options_get_value(data, 'table_optimize_font')
  if(!optimize_width){
    return(table_no_optimize_l(data = data))
  }
  if(!optimize_font){
    return(table_optimize_width_l(data = data))
  }

  table_optimize_width_font_l(data = data)
}

#' @noRd
create_caption_l <- function(data){
  if(!dt_has_caption(data)){
    return('')
  }
  cap <- dt_caption_get(data = data)
  paste0("\\caption{", cap$caption, "}\\\\[\\bigskipamount]")
}
#' Create the columns component of a table
#'
#' @noRd
create_columns_component_l <- function(data) {

  boxh <- dt_boxhead_get(data = data)
  stubh <- dt_stubhead_get(data = data)
  stub_available <- dt_stub_df_exists(data = data)
  spanners_present <- dt_spanners_exists(data = data)
  styles_tbl <- dt_styles_get(data = data)

  # Get the headings
  #headings <- boxh$column_label %>% unlist()

  headings_vars <- boxh %>% dplyr::filter(type == "default") %>% dplyr::pull(var)
  headings_labels <- dt_boxhead_get_vars_labels_default(data = data)

  # TODO: Implement hidden boxhead in LaTeX
  # # Should the column labels be hidden?
  # column_labels_hidden <-
  #   dt_options_get_value(data = data, option = "column_labels_hidden")
  #
  # if (column_labels_hidden) {
  #   return("")
  # }

  # If `stub_available` == TRUE, then replace with a set stubhead
  # label or nothing
  if (isTRUE(stub_available) && length(stubh$label) > 0) {

    #stubl <- fmt_latex_math(gsub("\\", "", stubh$label, fixed=TRUE))
    #stublabel <- style_stubhead_l(styles_tbl, stubl)

    headings_labels <- prepend_vec(headings_labels, stubh$label)
    headings_vars <- prepend_vec(headings_vars, "::stub")

  } else if (isTRUE(stub_available)) {

    headings_labels <- prepend_vec(headings_labels, "")
    headings_vars <- prepend_vec(headings_vars, "::stub")
  }


  table_col_headings <-
    paste0(latex_heading_row(content = headings_labels), collapse = "")

  if (spanners_present) {

    # Get vector of group labels (spanners)
    spanners <- dt_spanners_print(data = data, include_hidden = FALSE)

    # Promote column labels to the group level wherever the
    # spanner label is NA
    spanners[is.na(spanners)] <- headings_vars[!headings_vars == '::stub'][is.na(spanners)]

    if (stub_available) {
      spanners <- c(NA_character_, spanners)
    }

    spanners_lengths <- rle(spanners)

    multicol <- c()
    cmidrule <- c()

    for (i in seq(spanners_lengths$lengths)) {

      if (spanners_lengths$lengths[i] > 1) {

        if (length(multicol) > 0 &&
            grepl("\\\\multicolumn", multicol[length(multicol)])) {
          multicol <- c(multicol, "& ")
        }

        multicol <-
          c(multicol,
            paste0(
              "\\multicolumn{", spanners_lengths$lengths[i],
              "}{c}{",
              spanners_lengths$values[i],
              "} "))

        spanner_border <- paste0(
          "\\cmidrule(lr){",
          sum(spanners_lengths$lengths[1:i]) - spanners_lengths$lengths[i] + 1,
          "-",
          sum(spanners_lengths$lengths[1:i]),
          "}")

        #don't add line underneath empty spanners
        if(spanners_lengths$values[i] == ''){
          spanner_border <- ''
        }

        cmidrule <-
          c(cmidrule, spanner_border)


      } else {
        multicol <- c(multicol, "& ")
      }

    }


    multicol <- paste0(paste(multicol, collapse = ""), "\\\\ \n")
    cmidrule <- paste0(paste(cmidrule, collapse = ""), "\n")

    table_col_spanners <- paste(multicol, cmidrule, collapse = "")

  } else {

    table_col_spanners <- ""
  }

  paste0(table_col_spanners, table_col_headings)
}

create_summary_rows_l <- function(data){
  list_of_summaries <- dt_summary_df_get(data = data)

  hide_stub <- function(summary){
    to_hide <-
      data$`_boxhead`$var[data$`_boxhead`$type == 'hidden' |
                            data$`_boxhead`$type == 'stub']
    summary %>% dplyr::select(!to_hide)
  }

  merge_summary <- function(summary) {
      cols_merge <- data$`_col_merge`
      summary <- merge_summary_rows(summary, cols_merge)
      if (data$`_boxhead`$var[1] == 'rowname' &&
          data$`_boxhead`$type[1] == 'stub') {

        to_hide <- data$`_boxhead`$var[data$`_boxhead`$type == 'hidden']
        summary <- summary %>% dplyr::select(!to_hide)

      } else {
        summary <- hide_stub(summary)
      }

      summary
  }

  display_summaries <- list_of_summaries$summary_df_display_list
  if ("_col_merge" %in% names(data)) {

    summaries_merged <- purrr::map(names(display_summaries), function(.x){
      merged <- merge_summary(display_summaries[[.x]])
      merged[['::SUMMARY_NAME']] <- rep(.x, dim(merged)[1])
      merged
      })

  } else {

    summaries_merged <- purrr::map(names(display_summaries), function(.x){
      merged <- hide_stub(display_summaries[[.x]])
      merged[['::SUMMARY_NAME']] <- rep(.x, dim(merged)[1])
      merged
    })
  }

  summaries_tbl <- do.call(rbind, summaries_merged)
  to_format <- summaries_tbl %>%
    dplyr::select(-'::SUMMARY_NAME')

  df <- data.frame(lapply(to_format, function(.x){fmt_latex_math(.x) %>% extract('math_env')}))
  df[['::SUMMARY_KEY']] <- summaries_tbl[['::SUMMARY_NAME']]
  df

}

#' @noRd
create_body_rows_l <- function(data) {
  boxh <- dt_boxhead_get(data = data)
  styles_tbl <- dt_styles_get(data = data)
  body <- dt_body_get(data = data)
  summaries_present <- dt_summary_exists(data = data)
  list_of_summaries <- dt_summary_df_get(data = data)
  groups_rows_df <- dt_groups_rows_get(data = data)
  stub_components <- dt_stub_components(data = data)

  n_data_cols <- dt_boxhead_get_vars_default(data = data) %>% length()
  n_rows <- nrow(body)

  # Get the column alignments for the data columns (this
  # doesn't include the stub alignment)
  col_alignment <-
    boxh %>%
    dplyr::filter(type == "default") %>%
    dplyr::pull(column_align)

  # Get the column headings for the visible (e.g., `default`) columns
  default_vars <- dt_boxhead_get_vars_default(data = data)

  # TRUE when no stub and all summary
  # FALSE when stub and all summary
  # FALSE when no stub and no all summary
  # FALSE when stub and all-stub summary <- need
  # FALSE when stub and no all summary
  if ("rowname" %in% names(body)) {

    default_vars <- c("rowname", default_vars)

  }

  # Determine whether the stub is available through analysis
  # of the `stub_components`
  stub_available <- dt_stub_components_has_rowname(stub_components)

  if (stub_available) {
    n_cols <- n_data_cols + 1
  } else {
    n_cols <- n_data_cols
  }

  # Get the sequence of column numbers in the table body (these
  # are the visible columns in the table exclusive of the stub)
  column_series <- seq(n_cols)

  # Replace an NA group with an empty string
  if (any(is.na(groups_rows_df$group))) {

    groups_rows_df <-
      groups_rows_df %>%
      dplyr::mutate(
        group_label = ifelse(
          is.na(group_label), "\\vspace*{-5mm}", group_label)) %>%
      dplyr::mutate(
        group_label = gsub("^NA", "\\textemdash", group_label))
  }

  group_rows <- create_group_rows(n_rows, groups_rows_df, context = "latex", n_cols = n_cols)

  if (stub_available && !("rowname" %in% names(body))) {

    default_vars <- c("::rowname", default_vars)

    body <-
      dt_stub_df_get(data = data) %>%
      dplyr::select(rowname) %>%
      dplyr::rename(`::rowname` = rowname) %>%
      cbind(body)

  }

  # Split `body_content` by slices of rows and create data rows
  #body_content <- as.vector(t(body[, default_vars]))
  #body_content <- purrr::map_chr(body_content, function(.){fmt_latex_math(gsub("\\", "", ., fixed=TRUE))})
  #row_splits <- split(body_content, ceiling(seq_along(body_content) / n_cols))

  dt_show <- body[, default_vars]
  row_splits <- purrr::map(seq(dim(dt_show)[1]), ~unlist(dt_show[.x, ], use.names = FALSE))

  sum_rows <- NULL
  if (summaries_present) {
    sum_rows <- create_summary_rows_l(data = data)
  }

  list(group_rows = group_rows,
       row_splits = row_splits,
       n_rows = n_rows,
       sum_rows = sum_rows,
       groups_rows_df = groups_rows_df)
}

#' @noRd
create_body_component_l <- function(data) {

  rows <- create_body_rows_l(data = data)
  summary_positions <- rep('', rows$n_rows)

  data_rows <- create_data_rows(rows$n_rows, rows$row_splits, context = "latex")
  if (!is.null(rows$sum_rows)) {

    fmt_summary_rows <- function(.){
      paste0(apply(., c(1), paste, collapse = ' & '), ' \\\\ \n ')
    }

    collapse_rows <- function(.){
      paste0("\\midrule \n ", paste(c(.), collapse = ''))
    }

    rows$sum_rows$`::COLLAPSED` <- rows$sum_rows %>%
      dplyr::select(-'::SUMMARY_KEY') %>%
      fmt_summary_rows()

    summary_rows_tbl <- rows$sum_rows %>%
      dplyr::group_by(`::SUMMARY_KEY`) %>%
      dplyr::summarise_at('::COLLAPSED', .funs = list(FULL_ROW = collapse_rows))

    gp_rows <- rbind(rows$groups_rows_df, c('::GRAND_SUMMARY', '', 0, rows$n_rows))
    positions <- gp_rows[match(summary_rows_tbl[['::SUMMARY_KEY']], gp_rows$group),]$row_end %>% as.numeric()
    summary_positions[positions] <- summary_rows_tbl$FULL_ROW

  }

  paste(paste0(rows$group_rows, data_rows, summary_positions), collapse = "")
}

#' @noRd
create_table_end_l <- function() {
  paste0(
    "\\end{longtable}\n",
    "\\end{ThreePartTable}\n")
}


#' @noRd
create_source_foot_note_component_l <- function(data) {

  footnotes_tbl <- dt_footnotes_get(data = data)
  opts_df <- dt_options_get(data = data)

  # If the `footnotes_resolved` object has no
  # rows, then return an empty footnotes component
  if (nrow(footnotes_tbl) != 0) {

  footnotes_tbl <-
    footnotes_tbl %>%
    dplyr::select(fs_id, footnotes) %>%
    dplyr::distinct()

  # Get the separator option from `opts_df`
  separator <-
    opts_df %>%
    dplyr::filter(parameter == "footnotes_sep") %>%
    dplyr::pull(value)

  # Convert an HTML break tag to a Latex line break
  separator <-
    separator %>%
    tidy_gsub("<br\\s*?(/|)>", "\\newline") %>%
    tidy_gsub("&nbsp;", " ")

  footnotes <- footnotes_tbl[["footnotes"]] %>%
    unescape_html() %>%
    markdown_to_latex() %>%
    fmt_latex_math() %>%
    extract('math_env')

  footnotes <-  paste0(footnote_mark_to_latex(footnotes_tbl[["fs_id"]]),
                       footnotes)

  footnotes <- paste(paste0('\\item ',
                      footnotes,
                      '\n'),
                     collapse = '')
  } else {

    footnotes <- NULL

  }

  source_note <- dt_source_notes_get(data = data)


  # If the `footnotes_resolved` object has no
  # rows, then return an empty footnotes component
  if (length(source_note) != 0) {

    source_note <- fmt_latex_math(source_note) %>% extract('math_env')
    source_note <- paste(paste0('\\item ',
                              source_note,
                              '\n'),
                       collapse = '')
  } else {

    source_note <- NULL

  }

  footnote_align <- dt_options_get_value(data, 'footnotes_align')
  sourcenote_align <- dt_options_get_value(data, 'source_notes_align')
  align <- function(setting){

    if(is.na(setting)){
      return('\\arraybackslash\\raggedright\n')
    }

    switch(setting,
           left = '\\arraybackslash\\raggedright\n',
           right = '\\arraybackslash\\raggedleft\n',
           center = '\\centering')
  }

  footnotes_align <- align(footnote_align)
  sourcenotes_align <- align(sourcenote_align)
  inputs <- list(size = '\\footnotesize',
                 footnotes = footnotes,
                 sourcenotes = source_note,
                 footnotes_align = footnotes_align,
                 sourcenotes_align = sourcenotes_align)

  whisker::whisker.render(latex_templates$source_foot_notes, inputs)

}
