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
  cex = fontsize/12
  out <- tryCatch(
    {
    suppressMessages(tikzDevice:: getLatexStrWidth(latex_str, cex = cex)) *0.0352778
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
    '12' = NA
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

  lengths <-
    list(
      '11pt' = rlang::eval_tidy(func, list(x = 11)),
      '12pt' = rlang::eval_tidy(func, list(x = 12))
    )

  lengths.slope <- slope(lengths$`11pt`,
                         lengths$`12pt`,
                         11,
                         12)

  lengths.addl <- purrr::map(5:10, ~ .x * lengths.slope)
  names(lengths.addl) <- paste0(5:10, 'pt')
  append(lengths.addl, lengths)
}

lengths_tbl_summary <- function(lengths_tbl, pt_size) {
  per_col.max <- apply(lengths_tbl, 2, max)
  list(
    lengths_tbl = lengths_tbl,
    max = per_col.max,
    total_size = sum(per_col.max),
    fit.page = sum(per_col.max) < 17.6,
    pt = as.numeric(gsub('pt', '', pt_size))
  )
}

optimize_excess <- function(required_column_widths){

  if(length(required_column_widths) == 1){
    return(list(optimized = required_column_widths,
                linebreaks = 0))
  }

  full_width <- latex_cache$pagewidth[[latex_cache$orient]][1]*2.54
  page_width <- full_width - sum(latex_cache$margin)*2.54

  table_width <- sum(required_column_widths)
  diff <- page_width - table_width
  x0 <- rep(0, length(required_column_widths))

  ub <- rep(diff, length(required_column_widths))
  lb <- rep(0, length(required_column_widths))

  OF <- function(x0,required_column_widths) {stats::sd(x0 + required_column_widths)}
  hin <- function(x0){diff - sum(x0)}
  params <- nloptr::slsqp(x0 = x0,
                          OF,
                          hin = hin,
                          lower = lb,
                          upper = ub,
                          required_column_widths = required_column_widths,
                          control = list(xtol_rel = 1e-10,
                                         maxeval = 100000))
  #browser()
  list(optimized = params$par + required_column_widths,
       linebreaks = 0)
}

optimize_difference <- function(required_column_widths){
  full_width <- latex_cache$pagewidth[[latex_cache$orient]][1]*2.54
  page_width <- full_width - sum(latex_cache$margin)*2.54

  table_width <- sum(required_column_widths)
  diff <- table_width - page_width
  x0 <- rep(diff/length(required_column_widths), length(required_column_widths))

  ub <- rep(diff, length(required_column_widths))
  lb <- rep(0, length(required_column_widths))

  OF <- function(x0,required_column_widths) {stats::sd(required_column_widths - x0)}
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
  list(type_size = type_setting(best[[1]]$pt),
       header = fmt_header_latex(best[[1]]$optimized))
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
