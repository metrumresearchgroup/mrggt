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
latex_column_sep <- function(data){
  col_sep <-
    gsub('px', '', dt_options_get_value(data, 'column_sep')) %>% as.numeric()
  tbl_cache$col_sep <- round(col_sep*0.75, 1)
  paste0('{', tbl_cache$col_sep, 'pt}')
}


latex_border_cmd <- function(){
  if(is.null(tbl_cache$border_cmd)){
    return(NULL)
  } else {
    tbl_cache$border_cmd <- c('\\newcommand{\\resetborderstyle}{\\arrayrulecolor{black}}',
                              tbl_cache$border_cmd)
  }
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
  paste0("\\caption{", cap$caption, "}")
}

#' @noRd
create_overflow_message_l <- function(data){

  get_styled_message <- function(setting, message){

    switch(setting,
           "bold+italic" = paste0('\\textbf{\\textit{', message, '}}'),
           "italic" = paste0('\\textit{', message, '}'),
           "bold" = paste0('\\textbf', message, '}'))
  }

  if(dt_has_overflow(data)){

    dt_overflow <- dt_overflow_get(data)

    if(!is.null(dt_overflow$message)){
      inputs <- list(align = get_latex_col_align(dt_overflow$message.align),
                     message = get_styled_message(dt_overflow$message.style, dt_overflow$message),
                     num_cols = tbl_cache$num_cols)

      return(whisker::whisker.render(latex_templates$cont_next_page, inputs))

    }
  }

  return('')
}


#' @noRd
create_columns_component_l <- function(data) {

  boxh <- dt_boxhead_get(data = data)
  stubh <- dt_stubhead_get(data = data)
  stub_available <- dt_stub_df_exists(data = data)
  spanners_present <- dt_spanners_exists(data = data)
  styles_tbl <- dt_styles_get(data = data)
  summaries_present <- dt_summary_exists(data = data)

  headings_vars <- boxh %>% dplyr::filter(type == "default") %>% dplyr::pull(var)
  headings_labels <- dt_boxhead_get_vars_labels_default(data = data)

  if (isTRUE(stub_available) && length(stubh$label) > 0) {

    headings_labels <- prepend_vec(headings_labels, stubh$label)
    headings_vars <- prepend_vec(headings_vars, "::stub")

  } else if (isTRUE(stub_available) | (summaries_present && !stub_available)) {

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

  full_colheadings <- paste0(table_col_spanners, table_col_headings)

  if(dt_has_overflow(data)){

    if(dt_overflow_get(data)$repeat_column_labels){

      return(whisker::whisker.render(latex_templates$collabels_everypage,
                              list(col_labels = paste0(table_col_spanners,
                                                       table_col_headings))))
    }
  }

  return(whisker::whisker.render(latex_templates$collabels_firstpage,
                                 list(col_labels = paste0(table_col_spanners,
                                                          table_col_headings))))

}

create_summary_rows_l <- function(data) {
  list_of_summaries <- dt_summary_df_get(data = data)

  hide_stub <- function(summary) {
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
    summaries_merged <-
      purrr::map(names(display_summaries), function(.x) {
        merged <- merge_summary(display_summaries[[.x]])
        merged[['::SUMMARY_NAME']] <- rep(.x, dim(merged)[1])
        merged
      })

  } else {
    summaries_merged <-
      purrr::map(names(display_summaries), function(.x) {
        merged <- hide_stub(display_summaries[[.x]])
        merged[['::SUMMARY_NAME']] <- rep(.x, dim(merged)[1])
        merged
      })
  }

  summaries_tbl <- do.call(rbind, summaries_merged)
  to_format <- summaries_tbl %>%
    dplyr::select(-'::SUMMARY_NAME')

  df <- to_format %>% fmt_latex_math()
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

  # if there is a summary row available, but no stub and stubhead, offset the cols by 1:
  if(!dt_stub_df_exists(data) && !dt_stubhead_has_label(data) && summaries_present){
    n_cols <- n_cols + 1
    default_vars <- c("::rowname", default_vars)

    body <-
      dt_stub_df_get(data = data) %>%
      dplyr::select(rowname) %>%
      dplyr::rename(`::rowname` = rowname) %>%
      cbind(body)
  }

  group_rows <- create_group_rows(n_rows,
                                  groups_rows_df,
                                  context = "latex",
                                  n_cols = n_cols)

  if (stub_available && !("rowname" %in% names(body))) {

    default_vars <- c("::rowname", default_vars)

    body <-
      dt_stub_df_get(data = data) %>%
      dplyr::select(rowname) %>%
      dplyr::rename(`::rowname` = rowname) %>%
      cbind(body)

  }

  dt_show <- body[, default_vars]
  row_splits <- purrr::map(seq(dim(dt_show)[1]), function(.x){
    row_split <- unlist(dt_show[.x, ], use.names = FALSE)
    row_split[is.na(row_split)] <- ''
    row_split
  }
    )

  sum_rows <- NULL
  if (summaries_present) {
    sum_rows <- create_summary_rows_l(data = data)
  }

  list(group_rows = group_rows,
       row_splits = row_splits,
       n_rows = n_rows,
       n_cols = n_cols,
       sum_rows = sum_rows,
       groups_rows_df = groups_rows_df)
}


get_border_commands_tb <- function() {
  border_matrix <- tbl_cache$border_data_matrix
  tb_borders <-
    purrr::map_chr(seq(dim(border_matrix)[1]), function(rown) {
      border_row <- border_matrix[rown,]

      if(identical(unique(border_row), '~')){
        return('')
      }

      if(length(unique(border_row)) == 1){

        if(grepl("^\\d{1}$", unique(border_row))){
          return('COL')
        }
      }

      row_styles <- paste(border_row, collapse = '')
      return(paste0('\\hhline{',
                    row_styles,
                    '}\\resetborderstyle\n'))
    })

  if (identical(unique(tb_borders), '')) {
    return(NULL)
  }
  return(tb_borders)
}


#' @noRd
create_body_component_l <- function(data) {

  rows <- create_body_rows_l(data = data)
  summary_positions <- rep('', rows$n_rows)

  data_rows <- create_data_rows(rows$n_rows,
                                rows$row_splits,
                                context = "latex")

  if (!is.null(rows$sum_rows)) {

    if(dim(rows$sum_rows %>%
           dplyr::select(-'::SUMMARY_KEY'))[1] != rows$n_cols){

    }
    fmt_summary_rows <- function(.){
      paste0(apply(., c(1), paste, collapse = ' & '), ' \\\\ \n ')
    }

    collapse_rows <- function(.){
      paste0(paste(c(.), collapse = ''))
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

  borders <- get_border_commands_tb()
  borders[borders == 'COL'] <- R.utils::insert(data_rows,
                                               which(summary_positions != '') + 1,
                                               summary_positions[!summary_positions == ''])

  summary_locations <- which(borders == summary_positions[!summary_positions == ''])

  for(i in summary_locations){
    if(borders[i -1] == ""){
      borders[i - 1] <- "\\midrule \n "
    }
    if(borders[i + 1] == ""){
      borders[i + 1] <- "\\midrule \n "
    }
  }

  all_cols <- R.utils::insert(borders,
                  which(borders %in% data_rows[which(rows$group_rows != '')]) - 1,
                  rows$group_rows[rows$group_rows != ''])

  paste(all_cols[all_cols != ''], collapse = '')

}

#' @noRd
create_table_end_l <- function() {
  return("\\end{longtable}\n")
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

  footnotes <- paste(paste0(footnotes,
                      ' \\\\ \n'),
                     collapse = '')
  } else {

    footnotes <- NULL

  }

  source_note <- dt_source_notes_get(data = data)


  # If the `footnotes_resolved` object has no
  # rows, then return an empty footnotes component
  if (length(source_note) != 0) {

    source_note <- fmt_latex_math(source_note) %>% extract('math_env')
    source_note <- paste(paste0(source_note,
                                ' \\\\ \n'),
                       collapse = '')
  } else {

    source_note <- NULL

  }

  if(is.null(footnotes)){

    if(is.null(source_note)){

      return('')

    } else {

      inputs <- list(
        sourcenotes_align = get_latex_align(dt_options_get_value(data, 'source_notes_align')),
        sourcenotes_size = get_latex_font_size(dt_options_get_value(data, 'source_notes_font_size')),
        sourcenotes = source_note
      )
      return(whisker::whisker.render(latex_templates$sourcenotes, inputs))
    }

  } else {

    inputs <- list(
      footnotes_align = get_latex_align(dt_options_get_value(data, 'footnotes_align')),
      footnotes_size = get_latex_font_size(dt_options_get_value(data, 'footnotes_font_size')),
      footnotes = footnotes
    )

    if(is.null(source_note)){

      return(whisker::whisker.render(latex_templates$sourcenotes, inputs))

    } else{

      inputs$sourcenotes_align <- get_latex_align(dt_options_get_value(data, 'source_notes_align'))
      inputs$sourcenotes_size <- get_latex_font_size(dt_options_get_value(data, 'source_notes_font_size'))
      inputs$sourcenotes <- source_note

      return(whisker::whisker.render(latex_templates$source_footnotes, inputs))
    }

  }

}

create_landscape_start <- function(orient){
  if(orient){
    return(NULL)
  }
  return(paste("\\begin{landscape}",
        "\\pagestyle{empty}",
        sep = "\n"))
}

create_landscape_end <- function(orient){
  if(orient){
    return(NULL)
  }
  return(paste("\\end{landscape}"))
}
