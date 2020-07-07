#' @noRd
latex_font_size_tbl <- function(){
  font_size <- dplyr::tribble(
    ~Command,     ~ "10pt",   ~ "11pt",   ~ "12pt", ~html,       ~ func,
    "\\tiny",             5,       6,       6,       '',           rlang::quo(paste0('{\\tiny ', x, '}')),
    "\\scriptsize",       7,       8,       8,       'xx-small',   rlang::quo(paste0('{\\scriptsize ', x, '}')),
    "\\footnotesize",     8,       9,       10,      'x-small',    rlang::quo(paste0('{\\footnotesize ', x, '}')),
    "\\small",            9,       10,      10.95,   'small',      rlang::quo(paste0('{\\small ', x, '}')),
    "\\normalsize",       10,      10.95,   12,      '',           rlang::quo(paste0('{\\normalsize ', x, '}')),
    "\\large",            12,      12,      14.4,    'medium',     rlang::quo(paste0('{\\large ', x, '}')),
    "\\Large",            14.4,    14.4,    17.28,   'large',      rlang::quo(paste0('{\\Large ', x, '}')),
    "\\LARGE",            17.28,   17.28,   20.74,   'x-large',    rlang::quo(paste0('{\\LARGE ', x, '}')),
    "\\huge",             20.74,   20.74,   24.88,   'xx-large',   rlang::quo(paste0('{\\huge ', x, '}')),
    "\\Huge",             24.88,   24.88,   24.88,     '',        rlang::quo(paste0('{\\Huge ', x, '}'))
  )
}

#' @noRd
define_overline_latex <- function(){
  paste(
    "\\newsavebox\\OBox",
    "\\newcommand\\hoverline[2][0.5pt]{%",
    "\\ifmmode\\sbox\\OBox{$#2$}\\else\\sbox\\OBox{#2}\\fi%",
    "\\makebox[0pt][l]{\\usebox\\OBox}%",
    "\\rule[1.5\\ht\\OBox-#1/2]{\\wd\\OBox}{#1}}",
    sep = "\n"
  )
    }


#' @noRd
define_strikeout_latex <- function(){
  paste(
  "\\newsavebox\\CBox",
  "\\newcommand\\hcancel[2][0.5pt]{%",
  "\\ifmmode\\sbox\\CBox{$#2$}\\else\\sbox\\CBox{#2}\\fi%",
  "\\makebox[0pt][l]{\\usebox\\CBox}%",
  "\\rule[0.5\\ht\\CBox-#1/2]{\\wd\\CBox}{#1}}",
  sep = "\n"
  )
}

#' @noRd
get_latex_font_size <- function(size_value, convert = 'command') {
  fs_tbl <- latex_font_size_tbl()
  fs_page_num <- as.numeric(latex_cache$document_dec[2])
  fs_page <- paste0(fs_page_num, 'pt')

  to_num <- function(v){
    as.numeric(gsub('%|px', '', v))
  }

  if (!grepl('\\d', size_value)) {

    loc <- which(fs_tbl$html == size_value)
    font_spec <- fs_tbl[loc,]
    pt_size <- font_spec$`12pt`

  } else {

    if (grepl('%', size_value)) {

      pt_size <- fs_page_num * to_num(size_value) / 100

    } else {

      pt_size <- to_num(size_value) * 0.75

    }

    loc <- which.min(abs(pt_size - fs_tbl[[fs_page]]))
    font_spec <- fs_tbl[loc,]
  }

  if (convert == 'function') {

    return(font_spec$func[[1]])

  }

  if(convert == 'command'){

    return(font_spec$Command)

  }

  if(convert == 'pt_size'){

    return(pt_size)

  }
}


get_latex_align <- function(setting){

  if(is.na(setting)){
    return('\\arraybackslash\\raggedright\n')
  }

  switch(setting,
         left = '\\arraybackslash\\raggedright\n',
         right = '\\arraybackslash\\raggedleft\n',
         center = '\\centering')
}

get_latex_col_align <- function(setting){

  if(is.na(setting)){
    return('l')
  }

  switch(setting,
         left = 'l',
         right = 'r',
         center = 'c')
}



#construct the color definition latex code lines (required)
#user inputted hex colors must be translated to rgb
#' @noRd
create_color_definition <- function(color){
  if(startsWith(color, 'rgba')){

    rgba <- stringr::str_split(gsub('rgba|\\(|\\)', '', color), ',')[[1]] %>% as.numeric()
    rgb_overlay <- rgba[1:3]
    s <- (rgba[4]*rgba[1:3] + (1 - rgba[4])*c(255, 255, 255))/255

  } else {

    s <- grDevices::col2rgb(color)/255

  }

  new_color <- paste0(
    "\\definecolor{",
    gsub('#', '', color),
    "}{rgb}{",
    s[1],
    ",",
    s[2],
    ",",
    s[3],
    "} \n"
  )

  tbl_cache$color_def <- unique(c(tbl_cache$color_def, new_color))
}

alphanumeric_to_alpha <- function(phrase){

  translate <- list(on = '1',
                    tw = '2',
                    th = '3',
                    fo = '4',
                    fi = '5',
                    si = '6',
                    se = '7',
                    ei = '8',
                    ni = '9')
  for(alphanum in names(translate)){

    phrase <- gsub(translate[[alphanum]],
                   alphanum,
                   phrase)
  }

  stringr::str_extract_all(phrase,
                           '[a-zA-Z]') %>%
    unlist() %>%
    paste(collapse = '')
}
