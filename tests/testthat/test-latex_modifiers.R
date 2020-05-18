context("LaTeX -- Ensuring that LaTeX modifiers work as expected")

# Create a shorter version of `mtcars`
mtcars_short <- mtcars[1:2, ]

test_that("latex scaling table and individual columns", {

  # Create a `tbl_latex` object with `gt()`;
  # no columns are scaled
  tbl_gt <-
    gt(data = mtcars_short) %>%
    as_latex()

  # Create a `tbl_latex` object with `gt()`;
  # column 1 is scaled by 0.5, column 2 is scaled by 0.3
  tbl_gt2 <-
    gt(data = mtcars_short) %>%
    as_latex() %>%
    latex_scale(scale_factor = list(c(1, 0.5),
                                    c(2, 0.3)))

  # Create a `tbl_latex` object with `gt()`;
  # all columns are scaled by 0.4
  tbl_gt3 <-
    gt(data = mtcars_short) %>%
    as_latex() %>%
    latex_scale(scale_factor = 0.4)

  # extract the column lengths table 1
  lengths <- gsub("^p\\{|cm\\}",
                  "",
                  stringr::str_extract_all(tbl_gt,
                                           'p\\{\\d*\\.?\\d*cm\\}')[[1]]) %>%
    as.double()

  # extract the column lengths table 2
  lengths2 <- gsub("^p\\{|cm\\}",
                  "",
                  stringr::str_extract_all(tbl_gt2,
                                           'p\\{\\d*\\.?\\d*cm\\}')[[1]]) %>%
    as.double()

  # extract the column lengths table 3
  lengths3 <- gsub("^p\\{|cm\\}",
                   "",
                   stringr::str_extract_all(tbl_gt3,
                                            'p\\{\\d*\\.?\\d*cm\\}')[[1]]) %>%
    as.double()

  # calculate scale factors for column widths to go from table 1 to table 2
  scale_factors12 <- lengths2/lengths

  # Expect fixed values
  # scale factors for columns 1 & 2 should equal 0.5 & 0.3.
  expect_equal(round(scale_factors12[1:2], 1), c(0.5, 0.3))

  # Expect a fixed value
  # scale factors for other columns should be equal to 1 (unchanged)
  expect_equal(round(tail(scale_factors12, -2), 0), rep(1, length(scale_factors12) -2))

  # calculate scale factors for column widths to go from table 1 to table 3
  scale_factors13 <- lengths3/lengths

  # Expect a fixed value
  # scale factors for all columns should be equal to 0.4
  expect_equal(round(scale_factors13,1), rep(0.4, length(scale_factors13)))
}
)

test_that("latex landscape mode", {

  # Create a `tbl_latex` object with `gt()`;
  # add landscape orientation
  tbl_gt <-
    gt(data = mtcars_short) %>%
    lscape() %>%
    as_latex()

  # Expect a characteristic pattern
  # table should now be in a `lscape-mrggtab` environment
  expect_true(grepl("\\\\begin\\{landscape\\}", tbl_gt))
  expect_true(grepl("\\\\end\\{landscape\\}", tbl_gt))

  # Extract the knit meta from the knit_asis object
  # retrieve the extra lines for the last latex dep
  knit_meta <- attributes(tbl_gt)$knit_meta
  lscape_settings <- knit_meta[[length(knit_meta)]]$extra_lines

  # Expect fixed value
  # lscape-mrggtab environment is created & defined under last package dep
  expect_equal(
    lscape_settings,
    c(
      '\\newlength{\\hfoot}',
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
      '\\raisebox{\\hfoot}[0pt][0pt]{\\rlap{\\hspace{\\vfoot}\\rotatebox[origin=cB]{90}{\\thepage}}}\\fi}'
  ))


  # Create a `tbl_latex` object with `gt()`;
  # add table header 'Table 1', place in landscape mode
  tbl_gt <-
    gt(data = mtcars_short) %>%
    lscape() %>%
    tab_header(title = 'Table 1') %>%
    as_latex()

  # Create a `tbl_latex` object with `gt()`;
  # add table header 'Table 2', place in landscape mode
  tbl_gt2 <-
      gt(data = mtcars_short) %>%
      lscape() %>%
      tab_header(title = 'Table 2') %>%
      as_latex()

  # Create a `tbl_latex` object with `gt()`;
  # add table header 'Table 3', place in landscape mode
  tbl_gt3 <-
    gt(data = mtcars_short) %>%
    lscape() %>%
    tab_header(title = 'Table 3') %>%
    as_latex()

  # combine objects using s3 binary operator
  tbls <- tbl_gt + tbl_gt2 + tbl_gt3

  # Expect fixed classes
  # combining knit_asis; lscape_asis should return lscape_asis; knit_asis
  expect_true('knit_asis' %in% class(tbls))
  expect_true('lscape_asis' %in% class(tbls))
  expect_equal(length(class(tbls)), 2)

  # Expect a fixed attribute
  # combined tbls should have base attribute
  expect_true('base' %in% names(attributes(tbls)))


  # Expect a characteristic pattern
  # latex string should contain 'Table 1'
  expect_true(grepl('Table 1', tbls))

  # Expect a characteristic pattern
  # latex string should contain 'Table 2'
  expect_true(grepl('Table 2', tbls))

  # Expect a characteristic pattern
  # latex string should contain 'Table 3'
  expect_true(grepl('Table 3', tbls))

  # extract the positions of the longtable environ declarations
  ltbegin_pos <- gregexpr('\\\\begin\\{longtable\\}', tbls)[[1]]

  # extract the position of the landscape environ declaration
  lsbegin_pos <- gregexpr('\\\\begin\\{landscape\\}', tbls)[[1]]

  # Expect a fixed value
  # longtable environment appears three times
  expect_equal(length(ltbegin_pos), 3)

  # Expect a fixed value
  # landscape environment appears once
  expect_equal(length(lsbegin_pos), 1)

  # Expect a characteristic condition
  # landscape environment is declared before longtable environments
  expect_true(min(lsbegin_pos) < min(ltbegin_pos))

  # extract the positions of the longtable environ endings
  ltend_pos <- gregexpr('\\\\end\\{longtable\\}', tbls)[[1]]

  # extract the position of the landscape environ ending
  lsend_pos <- gregexpr('\\\\end\\{landscape\\}', tbls)[[1]]

  # Expect a fixed value
  # longtable environment end appears three times
  expect_equal(length(ltend_pos), 3)

  # Expect a fixed value
  # landscape environment end appears once
  expect_equal(length(lsend_pos), 1)

  # Expect a characteristic condition
  # landscape environment is ended after longtable environments are ended
  expect_true(max(ltend_pos)< max(lsend_pos))
}
)

test_that("latex mrggtOptions", {

  # Expect fixed values
  # mrggt has default margins of left 1 in and right 1 in
  expect_equal(latex_cache$margin, c(1, 1))

  # Expect fixed value
  # mrggt has default page size of 'letter'
  expect_equal(latex_cache$papersize, 'letter')

  # Expect fixed value
  # mrggt has default line breaks set to TRUE
  expect_true(latex_cache$line.breaks)

  # create a gt table under default margins and paper size
  tbl_gt <-
    gt(data = mtcars_short) %>%
    as_latex()

  # extract the column lengths
  lengths <- gsub("^p\\{|cm\\}",
                  "",
                  stringr::str_extract_all(tbl_gt,
                                           'p\\{\\d*\\.?\\d*cm\\}')[[1]]) %>%
    as.double()


  # change the margin size to 2 in right, 2 in left
  mrggtOptions('pagemargin' = c(2, 2))

  # Expect a fixed pattern
  # cache margin should now be 2 in, 2 in
  expect_equal(latex_cache$margin, c(2, 2))

  # create a gt table with default paper size and 2 in margins
  tbl_gt2 <-
    gt(data = mtcars_short) %>%
    as_latex()

  # extract the column lengths
  lengths2 <- gsub("^p\\{|cm\\}",
                  "",
                  stringr::str_extract_all(tbl_gt2,
                                           'p\\{\\d*\\.?\\d*cm\\}')[[1]]) %>%
    as.double()

  # Expect characteristic condition
  # sum(column lengths) should be less for table with larger margins
  expect_true(sum(lengths2) < sum(lengths))

  # Expect error
  # try to specify more than 2 margin settings
  expect_error(mrggtOptions('pagemargin' = c(2, 2, 2)))

  # change the papersize to legal
  mrggtOptions('papersize' = 'legal')

  # Expect fixed value
  # mrggt page size should be legal
  expect_equal(latex_cache$papersize, 'legal')

  # Expect a fixed value
  # page width should now match spec for legal page
  expect_equal(latex_cache$pagewidth, list(
    'portrait' = c(8.5, 14.0),
    'landscape' = c(14.0, 8.5)
  ))

  # Expect a fixed value
  # pagewidth cannot be changed by the user (must be set by page size)
  # passing in a parameter for pagewidth should leave the pagewidth in cache unchanged
  suppressMessages(mrggtOptions('pagewidth' = 'test'))
  expect_equal(latex_cache$pagewidth, list(
    'portrait' = c(8.5, 14.0),
    'landscape' = c(14.0, 8.5)
  ))

  # Expect error
  # specifying a page size that does not exist
  expect_error(mrggtOptions('papersize' = 'Jeff'))

  # reset
  mrggtOptions('pagemargin' = c(1, 1))
  mrggtOptions('papersize' = 'letter')
}
)

