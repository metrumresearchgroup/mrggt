get_data_rows_l <- function(data) {
  body <- dt_body_get(data = data)
  default_vars <- dt_boxhead_get_vars_default(data = data)
  n_data_cols <-
    dt_boxhead_get_vars_default(data = data) %>% length()
  stub_components <- dt_stub_components(data = data)
  stub_available <- dt_stub_components_has_rowname(stub_components)

  if (stub_available) {
    n_cols <- n_data_cols + 1
  } else {
    n_cols <- n_data_cols
  }

  if ("rowname" %in% names(body)) {
    default_vars <- c("rowname", default_vars)
  }

  if (stub_available) {
    default_vars <- c("::rowname", default_vars)

    body <-
      dt_stub_df_get(data = data) %>%
      dplyr::select(rowname) %>%
      dplyr::rename(`::rowname` = rowname) %>%
      cbind(body)
  }

  body_content <- as.vector(t(body[, default_vars]))
  row_splits <-
    split(body_content, ceiling(seq_along(body_content) / n_cols))
  data_rows <-
    do.call(rbind, lapply(row_splits, function(x)
      as.data.frame(t(x))))
  data_rows
}

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

sanitizeTikz <- function(latex_str){

  sanitize_list <- list(c("\\multicolumn{1}{l}", ''),
                        c("\\multicolumn{1}{c}", ''),
                        c("\\multicolumn{1}{r}", ''))

  if(length(tbl_cache$color) != 0){

    color_list <- append(purrr::map(tbl_cache$color, ~ c(paste0('\\cellcolor{',.x, '}'), '')),
                         purrr::map(tbl_cache$color, ~ c(paste0('\\textcolor{',.x, '}'), '')))

    sanitize_list <- append(sanitize_list,
                            color_list)
  }

  sanitize_list <- append(sanitize_list,
                          list(c('\\checkmark', 'V')))

  gsub_multiple(latex_str, sanitize_list)

}

find_chr_length <- function(latex_str, fontsize){
  latex_str <- sanitizeTikz(latex_str)
  cex <- fontsize/12
  latex_packages <- paste0('\\usepackage{', latex_packages(), '}')

  out <- tryCatch(
    {
    suppressMessages(tikzDevice::getLatexStrWidth(latex_str, cex = cex, packages = latex_packages)) *0.0352778
  },
  error=function(cond) {
    round(grid::convertWidth(grid::grobWidth(grid::textGrob(
      latex_str, gp = grid::gpar(fontsize = fontsize))), 'cm'), 2) %>% as.double()
  }
    )
 out
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
  #browser()
  sizing_options[[as.character(type_size)]]
}

slope <- function(y2, y1, x2, x1){
  (y2-y1)/(x2-x1)
}

find_lengths <- function(data.element){
  func <- quote(purrr::map_dbl(data.element,
                               find_chr_length,
                               fontsize = x))

  if(!is.null(dim(data.element))){
    func <- quote(apply(data.element,
                        c(1, 2),
                        find_chr_length,
                        fontsize = x))
  }

  # find the length of every element when font size = 12 pt and 11 pt
  lengths <-
    list(
      '11pt' = rlang::eval_tidy(func, list(x = 11)),
      '12pt' = rlang::eval_tidy(func, list(x = 12))
    )

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
  option
}

calculate_best <- function(data_rows, collabels){
  length.labels <- find_lengths(collabels)
  length.data <- find_lengths(data_rows)
  length.full <- purrr::map2(length.labels, length.data, ~ rbind(.x, .y))
  length.summary <- purrr::map2(length.full, names(length.full), lengths_tbl_summary)
  optimized_lengths <- purrr::map(length.summary, optimize_options)
  ranked <- purrr::map(optimized_lengths, rank_options)
  best <- ranked[purrr::map_dbl(ranked, ~.x$rank) == max(purrr::map_dbl(ranked, ~.x$rank))]
  if(length(best) > 1){
    best <- best[purrr::map_dbl(best, ~.x$pt) == max(purrr::map_dbl(best, ~.x$pt))]
  }

  tbl_cache$font_size <- best[[1]]$pt
  fmt_header_latex(best[[1]]$optimized)
}

fmt_header_latex <- function(sizing_columns){
 tbl_cache$tbl_width <- sum(sizing_columns)
 paste0('\\begin{longtable}{',
        paste(sprintf("p{%.2fcm}", sizing_columns), collapse = ''),
        '} \n')

}

calc_column_width_l <- function(data) {
  data_rows <- get_data_rows_l(data)
  collabels <- get_collabels_l(data)
  calculate_best(data_rows, collabels)
}
