#### Possible locations for style
## stub_groups
## stubhead
## columns_columns
## columns_groups
## data
## stub
## row_groups

#' @noRd
resolve_styles_latex <- function(data){
  styles_tbl <- dt_styles_get(data = data)
  boxh <- dt_boxhead_get(data = data)
  stub_available <- dt_stub_df_exists(data = data)
  spanners_present <- dt_spanners_exists(data = data)

  headings_vars <- data$`_boxhead`$var
  headings_labels <- data$`_boxhead`$column_label
  headings_labels <- purrr::map2(headings_vars,
                                 headings_labels,
                                 latex_style_headings,
                                  styles_df = styles_tbl)

  data$`_boxhead`$column_label <- headings_labels

  stubh <- dt_stubhead_get(data = data)

  if (isTRUE(stub_available) && length(stubh$label) > 0){
    stubh$label <- style_stubhead_l(styles_tbl, stubh$label)
    data <- dt_stubhead_set(data, stubh)
  }

  if (spanners_present){
    spanners <- dt_spanners_get(data)
    spanners$spanner_label <- purrr::map(spanners$spanner_label,
                                          latex_style_spanners,
                                          styles_tbl = styles_tbl)
    data <- dt_spanners_set(data, spanners)
  }

  groups_rows_df <- dt_groups_rows_get(data = data)
  groups_rows_df$group_label <- purrr::map_chr(groups_rows_df$group_label,
                                               style_group_rows_latex,
                                               styles_df = styles_tbl)

  data <- dt_groups_rows_set(data, groups_rows_df)

  stub <- data$`_stub_df`
  data$`_stub_df`$rowname <- purrr::map_chr(seq(dim(stub)[1]),
                                            ~latex_style_stub(stub[.x, ],
                                                              styles_tbl))

  body <- dt_body_get(data = data)
  body <- style_data_latex(body, styles_tbl)
  data <- dt_body_set(data, body)
  data
}

## row_groups
#' @noRd
style_group_rows_latex <- function(group_name, styles_df){
  to_style <- styles_df[styles_df$locname == 'row_groups',]
  gn.metadata <- fmt_latex_math(group_name)
  element <- gn.metadata %>% extract('math_env')

  if(group_name %in% to_style$grpname){
    gn.metadata$styles <- to_style[to_style$grpname == group_name, ]$styles[[1]]
    element <-  do.call(gn.metadata, latex_style_it)
  }

  element
}

## stubhead
#' @noRd
style_stubhead_l <- function(style_df, stubhead) {
  to_style <- style_df[style_df$locname == 'stubhead', ]
  sh.metadata <- fmt_latex_math(stubhead)
  element <- sh.metadata %>% extract('math_env')

  if(dim(to_style)[1] > 0){

    sh.metadata$styles <- to_style$styles[[1]]
    element <- do.call(latex_style_it, sh.metadata)

  }

  element
}

## columns_columns
#' @noRd
latex_style_headings <- function(headings_var, headings_label, styles_df) {
  to_style <- styles_df[styles_df$locname == 'columns_columns', ]
  hl.metadata <- fmt_latex_math(headings_label)
  element <- hl.metadata %>% extract('math_env')

  if(headings_var %in% to_style$colname){
    hl.metadata$styles <- to_style[to_style$colname == headings_var, ]$styles[[1]]
    hl.metadata$colnum <- to_style[to_style$colname == headings_var, ]$colnum
    element <- do.call(latex_style_it, hl.metadata)
  }

  element
}

## sub
#' @noRd
latex_style_stub <- function(stub, styles_tbl) {
  to_style <- styles_tbl[styles_tbl$locname == 'stub', ]
  stb.metadata <- fmt_latex_math(stub$rowname)
  element <- stb.metadata %>% extract('math_env')

  if(stub$rownum_i %in% to_style$rownum){
    stb.metadata$styles <- to_style[to_style$rownum == stub$rownum_i, ]$styles[[1]]
    element <- do.call(latex_style_it, stb.metadata)
  }

  element
}

## columns_groups
#' @noRd
latex_style_spanners <- function(label, styles_tbl) {
  to_style <- styles_tbl[styles_tbl$locname == 'columns_groups', ]
  if(is.na(label)){
    sp.metadata <- list(.Label = '',
                        math_env = '',
                        math_env2exp = rlang::quo(paste(x)))
  } else {
    sp.metadata <- fmt_latex_math(label)
  }

  element <- sp.metadata %>% extract('math_env')

  if(label %in% to_style$grpname){
    sp.metadata$styles <- to_style[to_style$grpname == label, ]$styles[[1]]
    element <- do.call(latex_style_it, sp.metadata)
  }

  element
}

## data & stub
#' @noRd
style_data_latex <- function(body, styles_df){
  to_style <- styles_df[styles_df$locname == 'data',]
  body.metadata <- fmt_latex_math(body)
  element <- body.metadata %>% extract('math_env')

  if(!dim(to_style)[1] > 0){
    return(element)
  }

  purrr::reduce(to_style %>% purrr::transpose(), function(.x, .y){
    with(.y, {
      cell.metadata <- body.metadata[[rownum, colnum]]
      cell.metadata$styles <- styles
      cell.metadata$colnum <- colnum
      .x[[rownum, colnum]] <- do.call(latex_style_it, cell.metadata)
      .x
    })
  },
  .init = element)
}


#' @noRd
latex_size_preset_values <- function(stringv){
  sizes <-
    list(
      xxsmall = rlang::quo(paste0('{\\scriptsize ', x, '}')),
      xsmall = rlang::quo(paste0('{\\footnotesize ', x, '}')),
      small =  rlang::quo(paste0('{\\small ', x, '}')),
      medium = rlang::quo(paste0('{\\large ', x, '}')),
      large =  rlang::quo(paste0('{\\large ', x, '}')),
      xlarge = rlang::quo(paste0('{\\LARGE ', x, '}')),
      xxlarge = rlang::quo(paste0('{\\huge ', x, '}'))
    )

  stringv <- tolower(gsub('-', '', stringv, fixed =TRUE))
  size <- sizes[[stringv]]
  if(is.null(sizes[[stringv]])){
    stringVector <- names(sizes)
    best_match <- stringVector[stringdist::amatch(stringv, stringVector, maxDist=Inf)]
    size <- sizes[[best_match]]
  }
  size
}

#user defined custom fontsize values
#' @noRd
latex_size_custom_values <- function(floatv){

  baselineskip <- as.character(1.2 * floatv)
  size <- as.character(floatv)
  exp2 <-
    paste0('{\\fontsize{', size, '}{', baselineskip, '}\\selectfont ')

  return(rlang::quo(paste0(exp2, x, '}')))
}

#determine if predefined or custom fontsize is being used
#' @noRd
latex_format_text_size = function(string){
  stripped <- gsub('px', '', string, fixed=TRUE)
  dblv <- suppressWarnings(as.double(stripped))

  if(is.na(dblv)){

    func <- latex_size_preset_values(stripped)

  } else {

    func <- latex_size_custom_values(dblv)

  }

  func
}

### Condensed text helper functions

#user defined condensed values
#' @noRd
latex_condensed_custom_values <- function(floatv) {

  percent_condensed <- as.character(floatv / 100)
  exp2 <- paste0('\\scalebox{', percent_condensed , '}{')

  return(rlang::quo(paste0(exp2, x, '}')))
}

#predefined condensed values
#' @noRd
latex_condensed_preset <- function(stringv){

  sizes <-
    list(
      ultracondensed = rlang::quo(paste0('\\textls[-90]{ ', x, '}')),
      extracondensed = rlang::quo(paste0('\\textls[-60]{', x, '}')),
      condensed =  rlang::quo(paste0('\\textls[-30]{', x, '}')),
      semicondensed = rlang::quo(paste0('\\scalebox{.9}{', x, '}')),
      semiexpanded =  rlang::quo(paste0('\\scalebox{1.1}{ ', x, '}')),
      expanded = rlang::quo(paste0('\\scalebox{1.3}{ ', x, '}')),
      extraexpanded = rlang::quo(paste0('\\scalebox{1.5}{ ', x, '}')),
      ultraexpanded = rlang::quo(paste0('\\scalebox{2}{ ', x, '}'))
    )

  stringVector <- names(sizes)
  best_match <- stringVector[stringdist::amatch(stringv, stringVector, maxDist=Inf)]
  sizes[[best_match]]
}

#determined if a predefined or custom condensed value is being used
#' @noRd
latex_format_condensed_size <- function(string){

  stripped <- gsub('%', '', string, fixed=TRUE)
  dblv <- suppressWarnings(as.double(stripped))

  if(is.na(dblv)){

    func <- latex_condensed_preset(stripped)

  } else {

    func <- latex_size_custom_values(dblv)

  }

  func
}

#' @noRd
cell_text.size <-  function(value){
  stripped <- gsub('px', '', value, fixed=TRUE)
  dblv <- suppressWarnings(as.double(stripped))

  if(is.na(dblv)){

    func <- latex_size_preset_values(stripped)

  } else {

    func <- latex_size_custom_values(dblv)

  }

  func
}

#' @noRd
cell_text.weight <- function(value){
  options <- list(bold = rlang::quo(paste0('\\textbf{', x, '}')),
                  bolder = rlang::quo(paste0('\\textbf{', x, '}')))

  if(value %in% names(options)){

    return(options[[value]])

  } else{

    return(rlang::quo(x))

  }
}

#' @noRd
cell_text.style <- function(value){
  options <- list(italic = rlang::quo(paste0('\\textit{', x, '}')),
                  center = rlang::quo(x),
                  normal = rlang::quo(x),
                  oblique = rlang::quo(paste0('\\textls{', x, '}')),
  )

  if(value %in% names(options)){

    return(options[[value]])

  } else{

    return(rlang::quo(x))

  }
}

#' @noRd
cell_text.transform <- function(value){
  options <- list(uppercase = rlang::quo(toupper(x)),
                  lowercase = rlang::quo(tolower(x)),
                  capitalize = rlang::quo(stringr::str_to_title(x)))

  if(value %in% names(options)){

    return(options[[value]])

  } else{

    return(rlang::quo(x))

  }
}


#' @noRd
cell_text.color <- function(value){
  color <- value

  if(startsWith(color, '#')){
    color <- gsub('#', '', color)
  }

  expl <- paste0('\\textcolor{', color, '}{')
  return(rlang::quo(paste0(expl, x, '}')))
}

#' @noRd
cell_text.align <- function(value){
  options <- list(
    center = rlang::quo(paste0('\\multicolumn{1}{c}{',
                               x,
                               '}')),
    left = rlang::quo(paste0('\\multicolumn{1}{l}{',
                             x,
                             '}')),
    right = rlang::quo(paste0('\\multicolumn{1}{r}{',
                              x,
                              '}'))
  )

  if(value %in% names(options)){

    return(options[[value]])

  } else{

    return(rlang::quo(x))

  }
}


#' @noRd
cell_text.decorate <- function(value){
  value <- gsub('-', '', value)
  options <- list(
    overline = rlang::quo(paste0('\\overline{', x, '}')),
    linethrough = rlang::quo(paste0('\\sout{', x, '}')),
    underline = rlang::quo(paste0('\\underline{', x, '}'))
  )

  if(value %in% names(options)){

    return(options[[value]])

  } else{

    return(rlang::quo(x))

  }
}

#' @noRd
cell_fill.color <- function(value){
  color <- value

  if(startsWith(color, '#')){
    color <- gsub('#', '', color)
  }

  expl <- paste0('\\cellcolor{', color, '}{')

  return(rlang::quo(paste0(expl, x, '}')))
}

#construct the color definition latex code lines (required)
#user inputted hex colors must be translated to rgb
#' @noRd
create_color_definition <- function(color){
  s <- grDevices::col2rgb(color)
  tbl_cache$color <- c(gsub('#', '', color), tbl_cache$color)
  paste0(
    "\\definecolor{",
    gsub('#', '', color),
    "}{rgb}{",
    s[1] / 255,
    ",",
    s[2] / 255,
    ",",
    s[3] / 255,
    "} \n"
  )
}

#determine if user needs a color definition in latex code (required)
#if the name 'colors' appears in the styles df, needs a color definition
#' @noRd
define_colors_latex <- function(styles_df) {
  v <- purrr::flatten(purrr::flatten(styles_df$styles))
  x <-
    as.data.frame(list(names = names(v), values = unlist(v, use.names = FALSE)))
  colors <- x[x$names == 'color',]$values

  if (length(colors) > 0) {

    paste(unique(purrr::map_chr(colors, create_color_definition)), collapse = '')

  } else {

    ''

  }
}

#' @noRd
get_latex_function_styles <- function(styles_list){
  to_apply <- unlist(styles_list)
  func_names <- names(to_apply)
  styling_functions <- purrr::map(func_names,
                                  ~ do.call(.x,
                                            args = list(value = to_apply[[.x]])))

  names(styling_functions) <- func_names
  styling_functions
}

#' @noRd
order_functions <- function(function.list){

  new_function_list <-
    function.list[!names(function.list) %in% c('cell_text.align', 'cell_text.transform')]

  # must be done last
  if('cell_text.align' %in% names(function.list)){
    new_function_list <- append(new_function_list, function.list['cell_text.align'])
  }

  #must be done first
  if('cell_text.transform' %in% names(function.list)){
    new_function_list <- append(function.list['cell_text.transform'], new_function_list)
  }

  new_function_list

}

#' @noRd
latex_style_it <- function(.Label, styles, math_env2exp, colnum = NULL, math_env = NULL) {
  funclist <- get_latex_function_styles(styles)
  funclist <- append(order_functions(funclist),
                     math_env2exp)

   purrr::reduce(funclist, function(.x, .y){
     rlang::eval_tidy(.y, list(x = .x))
     },
    .init = .Label)
}
