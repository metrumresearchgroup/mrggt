#### Possible locations for style
## stub_groups
## stubhead
## columns_columns
## columns_groups
## data
## stub
## row_groups

## row_groups
#' @noRd
style_group_rows_latex <- function(group_name, styles_df){
  to_style <- styles_df[styles_df$locname == 'row_groups',]

  if(group_name %in% to_style$grpname){

    styles <- to_style[to_style$grpname == group_name, ]$styles[[1]]
    group_name <-  latex_style_it(group_name, styles)

  }

  group_name
}

## stubhead
#' @noRd
style_stubhead_l <- function(style_df, stubhead) {
  to_style <- style_df[style_df$locname == 'stubhead', ]
  stubhead <- fmt_latex_math(gsub("\\", "", stubhead, fixed=TRUE))

  if(dim(to_style)[1] > 0){

    styles <- to_style$styles[[1]]
    stubhead <- latex_style_it(stubhead, styles)

  }

  stubhead
}

## columns_columns
#' @noRd
latex_style_headings <- function(headings_var, headings_label, styles_df) {
  to_style <- styles_df[styles_df$locname == 'columns_columns', ]
  headings_label <- fmt_latex_math(gsub("\\", "", headings_label, fixed=TRUE))

  if(headings_var %in% to_style$colname){
    label_metadata <- to_style[to_style$colname == headings_var, ]
    styles <- label_metadata$styles[[1]]
    headings_label <- latex_style_it(headings_label,
                                     styles,
                                     colnum = label_metadata$colnum)
  }
  headings_label
}

## columns_groups
#' @noRd
latex_style_spanners <- function(label, styles_tbl) {
  to_style <- styles_tbl[styles_tbl$locname == 'columns_groups', ]

  if(is.na(label)){
    label <- ''
  } else {
    label <- fmt_latex_math(gsub("\\", "", label, fixed=TRUE))
  }

  if(label %in% to_style$grpname){
    styles <- to_style[to_style$grpname == label, ]$styles[[1]]
    label <- latex_style_it(label, styles)
  }

  label
}

## data & stub
#' @noRd
style_data_latex <- function(row_splits, styles_df){
  to_style <- styles_df[styles_df$locname == 'data',]

  if(dim(to_style)[1] > 0){

    for (i in 1:dim(to_style)[1]) {

      styles <- to_style[i, ]$styles[[1]]
      colnum <- to_style[i, ]$colnum
      rownum <- as.character(to_style[i, ]$rownum)
      row <- row_splits[[rownum]]
      element <- row[colnum]
      row_splits[[rownum]][colnum] <- latex_style_it(element, styles, colnum)

    }
  }

  to_style <- styles_df[styles_df$locname == 'stub',]

  if(dim(to_style)[1] > 0){

    for (i in 1:dim(to_style)[1]) {

      styles <- to_style[i, ]$styles[[1]]
      row <- to_style[i, ]$rownum
      row_splits[[as.character(row)]][[1]] <-
        latex_style_it(row_splits[[as.character(row)]][[1]], styles)

    }
  }

  row_splits
}

### Font Size Helper Functions

#predefined gt fontsize values
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

### Highlight/Text Color Helper Functions

cell_text.size <-  function(cell_text){
  stripped <- gsub('px', '', cell_text$size, fixed=TRUE)
  dblv <- suppressWarnings(as.double(stripped))

  if(is.na(dblv)){

    func <- latex_size_preset_values(stripped)

  } else {

    func <- latex_size_custom_values(dblv)

  }

  func
}

cell_text.weight <- function(cell_text){
  value <- cell_text$weight
  options <- list(bold = rlang::quo(paste0('\\textbf{', x, '}')),
                  bolder = rlang::quo(paste0('\\textbf{', x, '}')))
  return(options[[value]])
}

cell_text.style <- function(cell_text){
  value <- cell_text$style
  options <- list(italic = rlang::quo(paste0('\\textit{', x, '}')),
                  center = rlang::quo(x),
                  normal = rlang::quo(x),
                  oblique = rlang::quo(paste0('\\textls{', x, '}')),
                  )
  return(options[[value]])
}

cell_text.transform <- function(cell_text){
  options <- list(uppercase = rlang::quo(toupper(x)),
                  lowercase = rlang::quo(tolower(x)),
                  capitalize = rlang::quo(stringr::str_to_title(x)))
  return(options[[cell_text$transform]])
}

cell_text.color <- function(cell_text){
  color <- cell_text$color

  if(startsWith(color, '#')){
    color <- gsub('#', '', color)
  }

  expl <- paste0('\\textcolor{', color, '}{')
  return(rlang::quo(paste0(expl, x, '}')))
}

cell_text.align <- function(cell_text){
  value <- cell_text$align
  options <- list(
    center = rlang::quo(paste0('\\mC{alignSUB}{',
                               x,
                               '}')),
    left = rlang::quo(paste0('\\mL{alignSUB}{',
                               x,
                               '}')),
    right = rlang::quo(paste0('\\mR{alignSUB}{',
                              x,
                              '}'))
  )
  options[[value]]
}

cell_text.decorate <- function(cell_text){
  value <- gsub('-', '', cell_text$decorate)
  options <- list(
    overline = rlang::quo(paste0('\\overline{', x, '}')),
    linethrough = rlang::quo(paste0('\\sout{', x, '}')),
    underline = rlang::quo(paste0('\\underline{', x, '}'))
  )
  options[[value]]
}

cell_fill.color <- function(cell_fill){
  color <- cell_fill$color

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

get_latex_function_styles <- function(styles_list){

  styling_functions <- purrr::map(names(unlist(styles_list)),
                                  R.utils::doCall,
                                  args = styles_list,
                                  .ignoreUnusedArgs = TRUE)

  names(styling_functions) <- names(unlist(styles_list))
  styling_functions
}

#' @noRd
order.functions <- function(function.list){

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
latex_style_it <- function(value, styles, colnum = NULL) {
  funclist <- get_latex_function_styles(styles)
  funclist <- order.functions(funclist)
  val <- list(x = value)

  for(function.name in names(funclist)){
    function.evaluated <- rlang::eval_tidy(funclist[[function.name]], val)

    if(function.name == 'cell_text.align'){
      col.command <- paste0('\\col', english::as.english(colnum))
      function.evaluated <- gsub('alignSUB',
                                 col.command,
                                 function.evaluated,
                                 fixed = TRUE)
    }

    val['x'] <- function.evaluated
  }
  val[['x']]
}

#' shrink latex
#' @param latex_code gt object
#' @param prop_shrink proportion to shrink
#' @param align align direction to align
#' @export
latex_shrink <- function(latex_code, prop_shrink, align = c('l', 'r', 'c')) {
  align <- match.arg(align)
  #Shrink the column widths
  tex_split <-
    unlist(stringr::str_split(latex_code %>% as.character(), '\n'))
  col_width_row <-
    tex_split[startsWith(tex_split, '\\begin{longtable}')]
  shrink_width <-
    as.double(unlist(
      qdapRegex::rm_between(col_width_row, 'p{', 'cm}', extract = TRUE)
    )) * prop_shrink

  if(is.null(align)){
    new_col_row <-
      paste0('\\begin{longtable}[c]{',
            paste(paste0(
              "p{", as.character(shrink_width), "cm}"
             ), collapse = ''),
            "}")
  } else{
    start <- paste0("\\begin{longtable}[", align, ']{')
    new_col_row <-
      paste0(start,
             paste(paste0(
               "p{", as.character(shrink_width), "cm}"
             ), collapse = ''),
             "}")
  }

  tex_split[startsWith(tex_split, '\\begin{longtable}')] <-
    new_col_row

  #Shrink the caption width
  cap_width_row <-
    tex_split[startsWith(tex_split, '\\setlength\\LTcapwidth')]
  shrink_cap <-
    as.double(unlist(
      qdapRegex::rm_between(cap_width_row, '{', 'cm}', extract = TRUE)
    )) * prop_shrink
  new_cap_row <-
    paste0('\\setlength\\LTcapwidth{', shrink_cap, 'cm}')
  tex_split[startsWith(tex_split, '\\setlength\\LTcapwidth')] <-
    new_cap_row

  tex_code <- paste(tex_split, collapse = '\n')
  latex_packages <- create_knit_meta(shrink = TRUE)
  tex_code %>% knitr::asis_output(meta = latex_packages)
}
