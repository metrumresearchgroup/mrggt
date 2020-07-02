#' @noRd
resolve_styles_latex <- function(data){

  data <- resolve_style_functions_latex(data = data)

  font_size <- data$`_options`$value[[which(data$`_options`$parameter == 'table_font_size')]]

  if(!font_size == '16px'){
    tbl_cache$font_size = font_size
  }

  data <- style_titles_latex(data = data)
  data <- style_group_rows_latex(data = data)
  data <- style_stubhead_latex(data = data)
  data <- style_headings_latex(data = data)
  data <- style_stub_latex(data = data)
  data <- style_spanners_latex(data = data)
  data <- style_body_latex(data = data)
  data
}

apply_style_wrap <- function(value, style_string){
  gsub('[::VALUE]', value, style_string, fixed = TRUE)
}

## row_groups
#' @noRd
style_group_rows_latex <- function(data){
  groups_rows_df <- dt_groups_rows_get(data = data)

  if(dim(groups_rows_df)[1] == 0){
    return(data)
  }

  group_labels <- groups_rows_df$group_label
  group_labels_math <- group_labels %>% fmt_latex_math()

  styles_tbl <- dt_styles_get(data = data)
  to_style <- styles_tbl[styles_tbl$locname == 'row_groups',]

  for(i in seq(length(group_labels))){

    if(group_labels[i] %in% to_style$grpname){
      style_wrap <- to_style[to_style$grpname == group_labels[i], ]$func
      group_labels_math[i] <- apply_style_wrap(group_labels_math[i],
                                               style_wrap)
    }
  }
  groups_rows_df$group_label <- group_labels_math
  data <- dt_groups_rows_set(data, groups_rows_df)
  data
}
## title
#' @noRd
style_titles_latex <- function(data){
  data$`_heading`$title <- data$`_heading`$title %>%
    fmt_latex_math()

  data$`_heading`$subtitle <- data$`_heading`$subtitle %>%
    fmt_latex_math()

  styles_tbl <- dt_styles_get(data = data)

  if('title' %in% styles_tbl$locname){

    sfunc <- styles_tbl[styles_tbl$locname == 'title',]$func
    data$`_heading`$title <- apply_style_wrap(data$`_heading`$title,
                     sfunc)
  }

  if('subtitle' %in% styles_tbl$locname){

    sfunc <- styles_tbl[styles_tbl$locname == 'subtitle',]$func
    data$`_heading`$subtitle <- apply_style_wrap(data$`_heading`$subtitle, sfunc)
  }

 data
}

## stubhead
#' @noRd
style_stubhead_latex <- function(data) {
  stubh <- dt_stubhead_get(data = data)
  stub_available <- dt_stub_df_exists(data = data)

  if (isTRUE(stub_available) && length(stubh$label) > 0){
    stubh$label <- stubh$label %>% fmt_latex_math()

    styles_tbl <- dt_styles_get(data = data)

    if('stubhead' %in% styles_tbl$locname){

     sfunc <- styles_tbl[styles_tbl$locname == 'stubhead', ]$func
     stubh$label <- apply_style_wrap(stubh$label, sfunc)
    }

    data <- dt_stubhead_set(data, stubh)

  }
  data
}

## columns_columns
#' @noRd
style_headings_latex <- function(data) {
  headings_vars <- data$`_boxhead`$var
  headings_labels <- data$`_boxhead`$column_label %>% fmt_latex_math()

  styles_tbl <- dt_styles_get(data = data)
  to_style <- styles_tbl[styles_tbl$locname == 'columns_columns', ]

  for(i in seq(length(headings_vars))){

    if(headings_vars[i] %in% to_style$colname){

      sfunc <- to_style[to_style$colname == headings_vars[i], ]$func
      headings_labels[i] <- apply_style_wrap(headings_labels[i],
                                             sfunc)
    }
  }

  data$`_boxhead`$column_label <- headings_labels
  data

}

## sub
#' @noRd
style_stub_latex <- function(data) {
  stub <- data$`_stub_df`
  rownames <- stub$rowname %>% fmt_latex_math()

  styles_tbl <- dt_styles_get(data = data)
  to_style <- styles_tbl[styles_tbl$locname == 'stub', ]

  for(i in seq(length(rownames))){

    if(stub[i,]$rownum_i %in% to_style$rownum){

      sfunc <- to_style[to_style$rownum == stub[i,]$rownum_i, ]$func
      rownames[i] <- apply_style_wrap(rownames[i], sfunc)
    }
  }
  data$`_stub_df`$rowname <- rownames
  data
}

## columns_groups
#' @noRd
style_spanners_latex <- function(data) {
  styles_tbl <- dt_styles_get(data = data)
  spanners_present <- dt_spanners_exists(data = data)

  if (spanners_present){
    spanners <- dt_spanners_get(data)
    spanner_labels <- spanners$spanner_label
    spanner_labels_math <- spanner_labels %>% fmt_latex_math()

    to_style <- styles_tbl[styles_tbl$locname == 'columns_groups', ]

    for(i in seq(length(spanner_labels))){
      if(spanner_labels[i] %in% to_style$grpname){
        spanner_labels_math[i] <- apply_style_wrap(spanner_labels_math[i],
                                                   to_style[to_style$grpname == spanner_labels[i], ]$func)
      }
    }

    spanners$spanner_label <- spanner_labels_math
    data <- dt_spanners_set(data, spanners)
  }
  data
}

## data & stub
#' @noRd
style_body_latex <- function(data){
  body <- dt_body_get(data = data)
  styles_tbl <- dt_styles_get(data = data)
  to_style <- styles_tbl[styles_tbl$locname == 'data',]
  body <- body %>% fmt_latex_math()

  if(!dim(to_style)[1] > 0){
    data <- dt_body_set(data, body)
    return(data)
  }

  for(i in seq(dim(to_style)[1])){
    rownum <- to_style[i,]$rownum
    colnum <- to_style[i,]$colnum
    sfunc <- to_style[i,]$func
    body[rownum, colnum] <- apply_style_wrap(body[rownum, colnum],
                                             sfunc)
  }

  data <- dt_body_set(data, body)
  data
}



cell_style_to_latex.cell_text <- function(styles, colnum, rownum, align, col_offset) {

  col_command <- paste0("\\col",
                        alphanumeric_to_alpha(colnum + col_offset))

  function_styles <- list(
    color = function(value) {

      create_color_definition(value)

      if (startsWith(value, '#')) {
        value <- gsub('#', '', value)
      }

      return(c(paste0('\\textcolor{',value, '}{'), '}'))
    },

    font = function(value) {
      return(c('', ''))
    },

    v_align = function(value) {
      tbl_cache$v_align <- unique(c(tbl_cache$v_align, colnum + col_offset))
      options <- list(
        top = paste0("\\makecell[t]{\\pbox{", col_command, "}{"),
        middle = paste0("\\makecell[c]{\\pbox{", col_command, "}{"),
        bottom = paste0("\\makecell[b]{\\pbox{", col_command, "}{")
      )

      return(c(options[[value]], " \\vspace{2mm}}}"))
    },

    size =  function(value) {
      c(paste0(get_latex_font_size(value), '{'), '}')
    },

    align = function(value) {
      options <- list(
        center = '\\multicolumn{1}{{::LEFT}c{::RIGHT}}{',
        left = '\\multicolumn{1}{{::LEFT}l{::RIGHT}}{',
        right = '\\multicolumn{1}{{::LEFT}r{::RIGHT}}{'
      )

      return(c(options[[value]], '}'))
    },

    ## v_align

    style = function(value) {
      options <- list(
        italic = "\\textit{",
        center = "\\textup{",
        normal = NULL,
        oblique = "\\textsl{"
      )
      if(is.null(options[[value]])){
        return(c('', ''))
      } else {
        return(c(options[[value]], '}'))
      }
    },

    weight = function(value) {
      if (!grepl('\\d', value)) {
        options <- list(
          bold = '\\textbf{',
          bolder = '\\textbf{',
          lighter = NULL,
          normal = NULL
        )

        if(is.null(options[[value]])){
          return(c('', ''))
        } else {
          return(c(options[[value]], '}'))
        }
      }

      return(c('\\textbf{', '}'))
    },

    indent = function(value) {
      value <- get_latex_font_size(value, 'pt_size')
      return(c(paste0("\\hspace{", value, "pt} "), ''))
    },


    # \usepackage[tracking=true]{microtype}
    stretch = function(value) {
      value <- gsub('-', '', value)
      if (!grepl('\\d', value)) {
        options <-
          list(
            ultracondensed = '\\textls[-150]{',
            extracondensed = '\\textls[-100]{',
            condensed =  '\\textls[-50]{',
            semicondensed ='\\textls[-25]{',
            semiexpanded =  '\\textls[25]{',
            expanded = '\\textls[50]{',
            extraexpanded = '\\textls[100]{',
            ultraexpanded = '\\textls[150]{'
          )

        return(c(options[[value]], '}'))
      }

      value <- gsub('%', '', value)
      return(c(paste0('\\textls[', value, ']{'), '}'))

    },


    transform = function(value) {
      options <-
        list(
          uppercase = "\\uppercase{",
          lowercase = "\\lowercase{",
          capitalize = "\\titlecap{"
        )

      return(c(options[[value]], '}'))
    },
    decorate = function(value) {
      value <- gsub('-', '', value)
      options <- list(
        overline = '\\hoverline{',
        linethrough = '\\hcancel{',
        underline = '\\underline{'
      )
      if(value == "overline"){
        tbl_cache$additional_cmds <- unique(c(tbl_cache$additional_cmds,
                                              define_overline_latex()))
      }
      if(value == 'linethrough'){
        tbl_cache$additional_cmds <- unique(c(tbl_cache$additional_cmds,
                                              define_strikeout_latex()))
      }
      return(c(options[[value]], '}'))
    }
  )


  func_list <- purrr::map(names(styles), ~ function_styles[[.x]](styles[[.x]]))
  names(func_list) <- dplyr::if_else(names(styles) == 'color', 'font_color', names(styles))
  func_list
}



cell_style_to_latex.cell_border <- function(styles, colnum, rownum, align, col_offset){
  if(styles$side == 'left'){
    return(list("::LEFT" =paste0(create_lr_borders(styles), ' ')))
  }
  if(styles$side == 'right'){
    return(list("::RIGHT" = paste0(' ', create_lr_borders(styles))))
  }
  if(styles$side == 'top'){
    return(map_tb_borders(styles, colnum, rownum, col_offset))
  }
  if(styles$side == 'bottom'){
    return(map_tb_borders(styles, colnum, rownum, col_offset))
  }
}


cell_style_to_latex.cell_fill <- function(styles, colnum, rownum, align, col_offset) {

  function_styles <- list(
    'color' = function(color) {

      create_color_definition(color)
      if (startsWith(color, '#')) {
        color <- gsub('#', '', color)
      }

      return(c(paste0('\\cellcolor{', color, '}{'), '}'))
    }
  )

  func_list <-
    purrr::map(names(styles), ~ function_styles[[.x]](styles[[.x]]))
  names(func_list) <- dplyr::if_else(names(styles) == 'color',
                                     'cell_color',
                                     names(styles))
  func_list
}


cell_style_to_latex.cell_styles <- function(styles, colnum, rownum, align, col_offset){
  styles_list <- purrr::flatten(purrr::map(styles,
                                           cell_style_to_latex,
                                           colnum = colnum,
                                           rownum = rownum,
                                           align = align,
                                           col_offset = col_offset))
  resolve_styles_list(styles_list, align)
}

cell_style_to_latex.default <- function(styles, colnum, rownumn, align, col_offset){
  purrr::map(styles, cell_style_to_latex, colnum = colnum, rownum = rownum, align = align, col_offset)
}

cell_style_to_latex <- function(styles, colnum, rownum, align, col_offset){
  UseMethod('cell_style_to_latex')
}

create_border_pos_matrix <- function(data){
  tbl <- get_data_rows_l(data)
  tbl <- tbl[rownames(tbl) != 'col_labels',]
  dim_tbl <- dim(tbl)

  row_add <- quote(rbind(rep('~', dim_tbl[2]),
                   rep(i, dim_tbl[2]),
                   rep('~', dim_tbl[2])))

  border_mat <- rlang::eval_tidy(row_add, list(i = 1))

  for(i in 2:dim_tbl[1]){
    border_mat <- rbind(border_mat, rlang::eval_tidy(row_add, list(i = i)))
  }

  tbl_cache$border_data_matrix <- border_mat
  tbl_cache$border_cols_matrix <-
    matrix(c(rep('~', dim_tbl[2]*2)),
           nrow = 2,
           ncol = dim_tbl[2])
}

create_lr_borders <- function(cell_border){
  create_color_definition(cell_border$color)
  color <- gsub('#', '', cell_border$color)
  weight <- paste0(cell_border$width %>%
                     get_latex_font_size('pt_size'), 'pt')

  command <- alphanumeric_to_alpha(paste0('v',
                                          color,
                                          weight))
  cmd_def <- paste0('\\newcommand{\\',
                    command,
                    '}{{\\color{',
                    color,
                    '}\\vline width ',
                    weight,
                    '}}')

  tbl_cache$border_cmd <- unique(c(tbl_cache$border_cmd, cmd_def))
  return(paste0('!\\', command))
}

create_tb_borders <- function(cell_border){
  create_color_definition(cell_border$color)
  color <- gsub('#', '', cell_border$color)
  pt_size <- cell_border$width %>% get_latex_font_size('pt_size')
  weight <- paste0(pt_size, 'pt')

  command <- alphanumeric_to_alpha(paste0('h',
                                          color,
                                          weight))

  rule_width <- paste0((pt_size - 0.01)/2, 'pt')
  cmd_def <- paste0("\\newcommand{\\",
                    command,
                    "}{\\arrayrulecolor{",
                    color,
                    "}\\setlength\\arrayrulewidth{",
                    rule_width,
                    "}\\setlength\\doublerulesep{0.01pt}}")

  cmd_def <- c('\\newcommand{\\resetborderstyle}{\\arrayrulecolor{black}}', cmd_def)

  tbl_cache$border_cmd <- unique(c(tbl_cache$border_cmd, cmd_def))
  return(paste0(">{\\", command, "}="))
}

map_tb_borders <- function(styles, colnum, rownum, col_offset){
  if(is.na(rownum)){

    if(is.na(colnum)){
      return(NULL)

    } else {
      rownum <- 2
      if(styles$side == 'top'){
        rownum <- 1
      }
      col_mat <- tbl_cache$border_cols_matrix
      col_mat[rownum, colnum + col_offset] <- create_tb_borders(styles)
      tbl_cache$border_cols_matrix <- col_mat
      return(NULL)
    }
  }

  border_matrix <-  tbl_cache$border_data_matrix
  border_command <- create_tb_borders(styles)
  rownum <- ceiling(rownum)
  position <- which(border_matrix[,1] == as.character(rownum))
  if(styles$side == 'bottom'){
    position <- position + 1
  }
  if(styles$side == 'top'){
    position <- position - 1
  }

  border_matrix[position, colnum + col_offset] <- border_command
  tbl_cache$border_data_matrix <- border_matrix
  return(NULL)
}


order_styles_list <- function(styles_list){
  ordered_functions <-
    c(
      "align",
      "cell_color",
      "v_align",
      "indent",
      "font_color",
      "stretch",
      "decorate",
      "size",
      "style",
      "weight",
      "transform"
    )
  styles_list[order(match(names(styles_list), ordered_functions))]
}

resolve_styles_list <- function(styles_list, align){
  if('align' %in% names(styles_list)){

    if('::RIGHT' %in% names(styles_list)){
      styles_list$align[1] <- gsub('{::RIGHT}',
                                   styles_list$`::RIGHT`,
                                   styles_list$align[1],
                                   fixed = TRUE)
    } else {
      styles_list$align[1] <- gsub('{::RIGHT}',
                                   '',
                                   styles_list$align[1],
                                   fixed = TRUE)
    }

    if('::LEFT' %in% names(styles_list)){
      styles_list$align[1] <- gsub('{::LEFT}',
                                   styles_list$`::LEFT`,
                                   styles_list$align[1],
                                   fixed = TRUE)
    } else {
      styles_list$align[1] <- gsub('{::LEFT}',
                                   '',
                                   styles_list$align[1],
                                   fixed = TRUE)
    }
  } else {

    if('::RIGHT' %in% names(styles_list)){

      if('::LEFT' %in% names(styles_list)){

        styles_list$align <- c(paste0("\\multicolumn{1}{",
                                      styles_list$`::LEFT`,
                                      align %>% substr(1, 1),
                                      styles_list$`::RIGHT`,
                                      "}{"),
                               "}")

      } else {

        styles_list$align <- c(paste0("\\multicolumn{1}{",
                                      align %>% substr(1, 1),
                                      styles_list$`::RIGHT`,
                                      "}{"),
                               "}")
      }

    } else {

      if('::LEFT' %in% names(styles_list)){

        styles_list$align <- c(paste0("\\multicolumn{1}{",
                                      styles_list$`::LEFT`,
                                      align %>% substr(1, 1),
                                      "}{"),
                               "}")
      }

    }
  }

  funcs <- styles_list[!names(styles_list) %in% c('::RIGHT', '::LEFT')]

  if(is.na(align)){
    funcs <- funcs[!names(funcs) %in% c('align')]
    if('cell_color' %in% names(funcs)){
      funcs$cell_color[1] <- gsub('\\cellcolor',
                                  '\\colorbox',
                                  funcs$cell_color[1],
                                  fixed = TRUE)
    }
    if('v_align' %in% names(funcs)){
      funcs$v_align[1] <- gsub("\\colNA", "\\LTwidth", funcs$v_align[1])
    }
  }

  funcs <- funcs %>%
    order_styles_list() %>%
    unlist(use.names = FALSE)

  expr1 <- paste(funcs[c(TRUE, FALSE)], collapse = '')
  expr2 <- paste(funcs[c(FALSE, TRUE)], collapse = '')
  paste0(expr1, "[::VALUE]", expr2)
}

check_column_offset <- function(data){
  if(dt_summary_exists(data)){
    if(!dt_stub_df_exists(data)){
      return(1)
    } else {
      return(0)
    }
  }
}

resolve_style_functions_latex <- function(data){
  styles_full <- data$`_styles`
  create_border_pos_matrix(data)
  align <- dt_boxhead_get(data)$column_align

  if(dt_stub_df_exists(data = data)){
    styles_full <- styles_full %>%
      dplyr::mutate(colnum = +1)
  }

  col_offset <- check_column_offset(data = data)

  styles_tbl <- styles_full %>%
    dplyr::mutate(align = align[colnum]) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(func = cell_style_to_latex(styles,
                                             colnum,
                                             rownum,
                                             align,
                                             col_offset)) %>%
    dplyr::ungroup()
  data$`_styles` <- styles_tbl
  data
}
