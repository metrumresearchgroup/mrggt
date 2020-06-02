
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

  body <- dt_body_get(data = data)
  to_hide <- data$`_boxhead`$var[data$`_boxhead`$type == 'hidden']
  body <- body %>% dplyr::select(!to_hide) %>% as.matrix()
  col_labels <- get_collabels_l(data)
  full_matrix <- rbind(col_labels, body)
  dimnames(full_matrix)[[2]] <- NULL
  summaries_present <- dt_summary_exists(data = data)

  if(summaries_present){
    sum_rows <- create_summary_rows_l(data = data)
    sum_rows <- as.matrix(sum_rows)
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

create_tex_width_file <- function(tbl_matrix){
  elements <- as.vector(tbl_matrix)
  keys <- paste0("\\", stringi::stri_rand_strings(length(elements),
                                                  7,
                                                  pattern = "[A-Za-z]"))

  vars_summary <- list(var_declarations = paste0("\\newlength{", keys, "} \n"),
                       var_assignments = paste0("\\settowidth{", keys, "}{", elements, "} \n"),
                       var_output =  paste0("\\message{^^J\\the", keys, "} \n"))

  vars_summary <- purrr::map(vars_summary,
                             paste,
                             collapse = '')

  vars_summary$color_declarations <- tbl_cache$color_def
  create_log_file(whisker::whisker.render(latex_templates$calc_width,
                                          vars_summary))
}


parse_tex <- function(tbl_matrix){
  log_file <- create_tex_width_file(tbl_matrix)
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

  names(sizes) <- c('11pt', '12pt')
  sizes
}

find_lengths <- function(tbl_matrix){
  tbl_matrix_clean <- sanitize_for_sizing(tbl_matrix)

  # find the length of every element when font size = 12 pt and 11 pt
  lengths <- parse_tex(tbl_matrix_clean)

  # use 11 pt and 12 calculation to determine how length changes with decreasing pt. for each element
  lengths.slope <- slope(lengths$`11pt`,
                         lengths$`12pt`,
                         11,
                         12)

  # assume linear relationship between pt. and output size.
  # use d/dx to calculate the 5-10 pt sizes for each element
  # (faster than using the tikzDevice for every single element at all pt sizes).

  lengths.addl <- purrr::map(5:10, ~ .x * lengths.slope)
  names(lengths.addl) <- paste0(5:10, 'pt')
  append(lengths.addl, lengths)
}

available_width_table <- function(number_columns){

  # available width = page width - (left margin + right margin) - number of columns *column separation
  # page width inverted for landscape orientation; first dimension is width in inches; 1 in -> 2.54 cm
  full_width <- latex_cache$pagewidth[[latex_cache$orient]][1]*2.54

  # sum(margins in inches)*2.54 -> margin in cm
  page_width <- full_width - sum(latex_cache$margin)*2.54

  # 3pt col sep per col; 1 pt -> 0.0352778 cm;
  page_width - 3*0.0352778*number_columns
}

lengths_tbl_summary <- function(lengths_tbl, pt_size) {
  page_width <- available_width_table(dim(lengths_tbl)[2])

  list(
    lengths_tbl = lengths_tbl,
    max = apply(lengths_tbl, 2, max),
    total_size = sum(apply(lengths_tbl, 2, max)),
    fit.page = sum(apply(lengths_tbl, 2, max)) < page_width,
    pt = as.numeric(gsub('pt', '', pt_size))
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
  rank_pt <- option$pt
  if(rank_pt > 8){
    rank_pt <- rank_pt - 2
  }
  option$rank <- rank_pt - option$linebreaks
  if(option$linebreaks == 0){
    option$rank = option$rank + 1
  }
  option
}

calculate_best <- function(tbl_matrix){
  tbl_matrix_lengths <- find_lengths(tbl_matrix)
  length_summary <- purrr::map2(tbl_matrix_lengths, names(tbl_matrix_lengths), lengths_tbl_summary)
  optimized_lengths <- purrr::map(length_summary, optimize_options)
  ranked <- purrr::map(optimized_lengths, rank_options)
  best <- ranked[purrr::map_dbl(ranked, ~.x$rank) == max(purrr::map_dbl(ranked, ~.x$rank))]
  if(length(best) > 1){
    best <- best[purrr::map_dbl(best, ~.x$pt) == max(purrr::map_dbl(best, ~.x$pt))]
  }

  tbl_cache$font_size <- best[[1]]$pt
  fmt_header_latex(best[[1]]$optimized)
}

fmt_header_latex <- function(sizing_columns){
 paste0('\\begin{longtable}{',
        paste(sprintf("p{%.2fcm}", sizing_columns), collapse = ''),
        '} \n')

}

calc_column_width_l <- function(data) {
  tbl_matrix <- get_data_rows_l(data)
  calculate_best(tbl_matrix)
}
