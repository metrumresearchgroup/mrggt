
get_collabels_l <- function(data) {
  stubh <- dt_stubhead_get(data = data)
  stub_available <- dt_stub_df_exists(data = data)
  collabels <- dt_boxhead_get_vars_labels_default(data = data)

  if (isTRUE(stub_available) && length(stubh$label) > 0) {
    collabels <- append(stubh$label, collabels)
  } else if (isTRUE(stub_available)) {
    collabels <- append('', collabels)
  }
  collabels
}

get_data_rows_l <- function(data) {
  rows <- create_body_rows_l(data = data)
  data_rows <- do.call(rbind, rows$row_splits)
  col_labels <- get_collabels_l(data)
  full_matrix <- rbind(col_labels, data_rows)
  dimnames(full_matrix)[[2]] <- NULL
  if(!is.null(rows$sum_rows)){
    sum_rows <- as.matrix(rows$sum_rows %>% dplyr::select(-'::SUMMARY_KEY'))
    dimnames(sum_rows)[[2]] <- NULL
    full_matrix <- rbind(full_matrix, sum_rows)
  }
  full_matrix
}

sanitize_for_sizing <- function(tbl_matrix){
  rgx <- "(^\\\\multicolumn\\{\\d{1}\\}\\{(?:l|r|c)\\}{1}\\{)"
  rgx2 <- "((?:\\}$|\\}\\s+$))"
  has_mulitcolumn <- tbl_matrix[grepl(rgx, tbl_matrix)]
  tbl_matrix[grepl(rgx, tbl_matrix)] <- gsub(rgx2, '', gsub(rgx, '', has_mulitcolumn))
  tbl_matrix
}

type_setting <- function(type_size) {
  sizing_options <- list(
    '5' = '\\tiny \n',
    '6' = '\\tiny \n',
    '7' = '\\tiny \n',
    '8' = '\\scriptsize \n',
    '9' = '\\scriptsize \n',
    '10' = '\\footnotesize \n',
    '11' = '\\small \n',
    '12' = "\n"
  )

  sizing_options[[as.character(type_size)]]
}

slope <- function(y2, y1, x2, x1){
  (y2-y1)/(x2-x1)
}


create_log_file <- function(tex_str){
  tmp <- tempfile(fileext = '.tex')
  readr::write_file(tex_str, tmp)
  withr::with_dir(dirname(tmp), {
    tmp_rendered <- tinytex::latexmk(basename(tmp), engine = 'pdflatex', clean = FALSE)
    log_file <- normalizePath(paste0(tools::file_path_sans_ext(tmp_rendered), '.log'))
  })
  log_file
}

create_tex_width_file <- function(tbl_matrix, font_size = NULL){
  elements <- as.vector(tbl_matrix)
  keys <- paste0("\\", stringi::stri_rand_strings(length(elements),
                                                  7,
                                                  pattern = "[A-Za-z]"))

  vars_summary <- list(var_assignments = paste0("\\settowidth{", keys, "}{", elements, "} \n"),
                       var_output =  paste0("\\message{^^J\\the", keys, "} \n"))

  vars_summary <- purrr::map(vars_summary,
                             paste,
                             collapse = '')

  sizes <- latex_font_size_tbl()$Command[1:5]
  if(is.null(font_size)){

    size_environments <- purrr::map_chr(sizes, function(.x){
      vars_summary$font_size_dec <- gsub('\\', '', .x, fixed = TRUE)
      vars_summary$font_size <- .x
      whisker::whisker.render(latex_templates$font_size_width,
                              vars_summary)
    })
    vars_summary <- list(font_size_width = paste(size_environments,
                                                 collapse = ' \n'))
  } else {
    vars_summary$font_size <- font_size
    vars_summary$font_size_dec <- gsub('\\', '', font_size, fixed = TRUE)
    vars_summary <- list(font_size_width = whisker::whisker.render(latex_templates$font_size_width,
                                                                   vars_summary))
  }

  vars_summary$var_declarations <- paste(paste0("\\newlength{", keys, "} \n"), collapse = '')
  vars_summary$color_declarations <- tbl_cache$color_def


  create_log_file(whisker::whisker.render(latex_templates$calc_width_template,
                                          vars_summary))
}


find_lengths <- function(tbl_matrix, font_size = NULL){
  tbl_matrix <- sanitize_for_sizing(tbl_matrix)
  log_file <- create_tex_width_file(tbl_matrix, font_size)
  tex_lines <- readr::read_lines(log_file)

  rangev <- mapply(`:`,
                   which(grepl('BEGINWIDTHS=', tex_lines)),
                   which(grepl('ENDWIDTHS=', tex_lines)),
                   SIMPLIFY = FALSE)

  nrows <- dim(tbl_matrix)[1]
  sizes <- lapply(rangev, function(x){
    # perfect latex this looks like:
    size <-
      unlist(stringr::str_extract_all(
        paste(tex_lines[x], collapse = ' '),
        "([+-]?(?:\\d*\\.)?\\d+\\w?pt)"
      ))

    matrix(as.numeric(gsub('pt', '', size))*0.0352778,
           nrow = nrows)
  })

  if(is.null(font_size)){
    names(sizes) <- gsub('\\', '', latex_font_size_tbl()$Command[1:5], fixed = TRUE)
  } else{
    names(sizes) <- gsub('\\', '', font_size, fixed = TRUE)
  }

  #names(sizes) <- c('11pt', '12pt')
  sizes
}

# find_lengths <- function(tbl_matrix, font_size){
#   tbl_matrix_clean <- sanitize_for_sizing(tbl_matrix)
#
#   # find the length of every element when font size = 12 pt and 11 pt
#   lengths <- parse_tex(tbl_matrix_clean)
#
#   # use 11 pt and 12 calculation to determine how length changes with decreasing pt. for each element
#   lengths.slope <- slope(lengths$`11pt`,
#                          lengths$`12pt`,
#                          11,
#                          12)
#
#   # assume linear relationship between pt. and output size.
#   # use d/dx to calculate the 5-10 pt sizes for each element
#   # (faster than using the tikzDevice for every single element at all pt sizes).
#   browser()
#   if(!is.null(font_size)){
#
#     one_font <- list(font_size*lengths.slope)
#     names(one_font) <- paste0(font_size, 'pt')
#     return(one_font)
#
#   }
#   lengths.addl <- purrr::map(5:10, ~ .x * lengths.slope)
#   names(lengths.addl) <- paste0(5:10, 'pt')
#   append(lengths.addl, lengths)
# }

available_width_table <- function(number_columns){

  # available width = page width - (left margin + right margin) - number of columns *column separation
  # page width inverted for landscape orientation; first dimension is width in inches; 1 in -> 2.54 cm
  full_width <- latex_cache$pagewidth[[latex_cache$orient]][1]*2.54

  # sum(margins in inches)*2.54 -> margin in cm
  page_width <- full_width - sum(latex_cache$margin)*2.54

  # 3pt col sep per col; 1 pt -> 0.0352778 cm;
  page_width - tbl_cache$col_sep*0.0352778*number_columns
}

lengths_tbl_summary <- function(lengths_tbl, type_setting) {
  page_width <- available_width_table(dim(lengths_tbl)[2])

  list(
    lengths_tbl = lengths_tbl,
    max = apply(lengths_tbl, 2, max),
    total_size = sum(apply(lengths_tbl, 2, max)),
    fit.page = sum(apply(lengths_tbl, 2, max)) < page_width,
    type_setting = paste0('\\', type_setting)
  )
}

optimize_excess <- function(required_column_widths){
  ## if table width is less than avail page width

  num_cols <- length(required_column_widths)

  if(num_cols == 1){
    return(list(optimized = required_column_widths,
                linebreaks = 0))
  }

  page_width <- available_width_table(num_cols)

  # max width the table needs to prevent line breaks
  table_width <- sum(required_column_widths)

  # how much excess is on the page
  diff <- page_width - table_width

  # start with x0 = 0; leave all table widths unchanged
  x0 <- rep(0, num_cols)

  # the max you can add to any column = page_width - table_width
  ub <- rep(diff, num_cols)

  # the min you can add is 0 (leave unchanged)
  lb <- rep(0, num_cols)

  # objective is to minimize the standard deviation between the column widths
  OF <- function(x0,required_column_widths) {stats::sd(x0 + required_column_widths)}

  # constrait sum(what is added to each column width) must be less than or equal to diff
  hin <- function(x0){diff - sum(x0)}

  params <- nloptr::slsqp(x0 = x0,
                          OF,
                          hin = hin,
                          lower = lb,
                          upper = ub,
                          required_column_widths = required_column_widths,
                          control = list(xtol_rel = 1e-10,
                                         maxeval = 100000))

  list(optimized = params$par + required_column_widths,
       linebreaks = 0)
}

optimize_difference <- function(required_column_widths){
  ## if table width is greater than avail page width

  num_cols <- length(required_column_widths)

  page_width <- available_width_table(num_cols)
  table_width <- sum(required_column_widths)

  diff <- table_width - page_width
  x0 <- rep(diff/num_cols, num_cols)

  ub <- rep(diff, num_cols)
  lb <- rep(0, num_cols)

  OF <- function(x0, required_column_widths) {stats::sd(required_column_widths - x0)}
  heq <- function(x0){sum(x0) - diff}

  params <- nloptr::slsqp(x0 = x0,
                          OF,
                          heq = heq,
                          lower = lb,
                          upper = ub,
                          required_column_widths = required_column_widths,
                          control = list(xtol_rel = 1e-10,
                                         maxeval = 100000))
  diffs <- round(params$par, 3)
  list(optimized = required_column_widths - diffs,
       linebreaks = length(diffs[diffs != 0]))
}

optimize_options <- function(summary){
  if(summary$fit.page){
    optimized = suppressMessages(optimize_excess(summary$max))
    summary$optimized = optimized$optimized
    summary$linebreaks = optimized$linebreaks
  }else{
    optimized = suppressMessages(optimize_difference(summary$max))
    summary$optimized = optimized$optimized
    summary$linebreaks = optimized$linebreaks
  }
  summary
}

rank_options <- function(option){
  rank_pt <- match(option$type_setting,
                   latex_font_size_tbl()$Command)

  option$font_index <- rank_pt
  if(rank_pt > 4){
    rank_pt <- rank_pt + 1
  }

  option$rank <- rank_pt - (option$linebreaks/length(option$optimized))*rank_pt
  if(option$linebreaks < 1){
    option$rank = option$rank + 1
  }
  option
}

find_optimize_options_l <- function(tbl_matrix, font_size = NULL){
  tbl_matrix_lengths <- find_lengths(tbl_matrix, font_size)
  length_summary <- purrr::map2(tbl_matrix_lengths,
                                names(tbl_matrix_lengths),
                                lengths_tbl_summary)
  purrr::map(length_summary, optimize_options)
}

calculate_best <- function(tbl_matrix){
  optimized_lengths <- find_optimize_options_l(tbl_matrix)
  ranked <- purrr::map(optimized_lengths, rank_options)
  best <-
    ranked[purrr::map_dbl(ranked, ~ .x$rank) == max(purrr::map_dbl(ranked, ~
                                                                     .x$rank))]
  if (length(best) > 1) {
    best <-
      best[purrr::map_dbl(best, ~ .x$font_index) == min(purrr::map_dbl(best, ~ abs(5 -.x$font_index)))]
  }

  tbl_cache$font_size <- best[[1]]$type_setting
  fmt_header_latex(best[[1]]$optimized)
}

fmt_header_latex <- function(sizing_columns){
 tbl_cache$num_cols <- length(sizing_columns)
 paste0('\\begin{longtable}{',
        paste(sprintf("p{%.2fcm}", sizing_columns), collapse = ''),
        '} \n')

}

table_optimize_width_font_l <- function(data) {
  tbl_matrix <- get_data_rows_l(data)
  calculate_best(tbl_matrix)
}


table_optimize_width_l <- function(data) {
  tbl_matrix <- get_data_rows_l(data)

  font_size <- data %>%
    dt_options_get_value('table_font_size') %>%
    get_latex_font_size()

  tbl_cache$font_size <- font_size

  optimized <- find_optimize_options_l(tbl_matrix, font_size)

  fmt_header_latex(optimized[[1]]$optimized)
}

table_no_optimize_l <- function(data){

  tbl_cache$font_size <- data %>%
    dt_options_get_value('table_font_size') %>%
    get_latex_font_size()

  col_alignment <-
    dt_boxhead_get(data = data) %>%
    dplyr::filter(type == "default") %>%
    dplyr::pull(column_align)

  # TODO: ensure that number of alignment tabs is correct
  if (dt_stub_df_exists(data = data)) {
    col_alignment <- c("left", col_alignment)
  }

  alignments <- col_alignment %>%
    substr(1, 1)

  tbl_cache$num_cols <- length(alignments)

  paste0(
    "\\begin{longtable}{",
    alignments %>% paste(collapse = ""),
    "}\n",
    collapse = ""
  )

}


