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
  expect_equal(round(tail(scale_factors12, -2), 1), rep(1, length(scale_factors12) -2))

  # calculate scale factors for column widths to go from table 1 to table 3
  scale_factors13 <- lengths3/lengths

  # Expect a fixed value
  # scale factors for all columns should be equal to 0.4
  expect_equal(round(scale_factors13, 1), rep(0.4, length(scale_factors13)))
}
)

test_that("latex landscape mode", {

  # Create a `tbl_latex` object with `gt()`;
  # add landscape orientation
  tbl_gt <-
    gt(data = mtcars_short) %>%
    as_latex() %>%
    lscape()

  # Expect a characteristic pattern
  # table should now be in a `lscape-mrggtab` environment
  expect_true(grepl("\\\\begin\\{lscape-mrggtab\\}", tbl_gt))
  expect_true(grepl("\\\\end\\{lscape-mrggtab\\}", tbl_gt))

  # Expect a characteristic pattern
  # table should not be in a longtable environment
  expect_false(grepl("\\\\begin\\{longtable\\}", tbl_gt))
  expect_false(grepl("\\\\end\\{longtable\\}", tbl_gt))

  # Extract the knit meta from the knit_asis object
  # retrieve the extra lines for the last latex dep
  knit_meta <- attributes(tbl_gt)$knit_meta
  lscape_def <- knit_meta[[length(knit_meta)]]$extra_lines

  # Expect fixed value
  # lscape-mrggtab environment is created & defined under last package dep
  expect_equal(
    lscape_def,
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
      '\\raisebox{\\hfoot}[0pt][0pt]{\\rlap{\\hspace{\\vfoot}\\rotatebox[origin=cB]{90}{\\thepage}}}\\fi}',
      '\\newenvironment{lscape-mrggtab}[2][1.5pt]',
      '{',
      '\\begin{landscape}',
      "\\pagestyle{empty}",
      #"\\setlength\\LTleft{-.75cm}",
      #"\\setlength\\LTright{0pt plus 1fill minus 1fill}",
      #"\\setlength\\LTcapwidth{18cm}",
      '\\begin{longtable}{#2}',
      '}',
      '{',
      '\\end{longtable}',
      '\\end{landscape}',
      '}'
    )
  )


  # Create a `tbl_latex` object with `gt()`;
  # add table header 'Table 1'
  tbl_gt <-
    gt(data = mtcars_short) %>%
    tab_header(title = 'Table 1') %>%
    as_latex()

  # Create a `tbl_latex` object with `gt()`;
  # add table header 'Table 2'
  tbl_gt2 <-
      gt(data = mtcars_short) %>%
      tab_header(title = 'Table 2') %>%
      as_latex()

  # Create a `tbl_latex` object with `gt()`;
  # add table header 'Table 3'
  tbl_gt3 <-
    gt(data = mtcars_short) %>%
    tab_header(title = 'Table 3') %>%
    as_latex()

  # combine objects using s3 binary operator
  tbls <- tbl_gt + tbl_gt2 + tbl_gt3

  # Expect fixed class
  # combining knit_asis using '+' should return a list
  expect_true(class(tbls) == 'list')

  # Expect a fixed value
  # combining 3 knit_asis should return a list of length 3
  expect_equal(length(tbls), 3)

  # Expect a characteristic pattern
  # first element of list should contain 'Table 1'
  expect_true(grepl('Table 1', tbls[[1]]))

  # Expect a characteristic pattern
  # second element of list should contain 'Table 2'
  expect_true(grepl('Table 2', tbls[[2]]))

  # Expect a characteristic pattern
  # third element of list should contain 'Table 3'
  expect_true(grepl('Table 3', tbls[[3]]))

  # put the combined tables in landscape mode
  lscape_tbls <- tbls %>% lscape()

  # Expect fixed class
  # lscape should collapse list and return knit_asis
  expect_true(class(lscape_tbls) == 'knit_asis')

  # extract the positions of the longtable environ declarations
  ltbegin_pos <- gregexpr('\\\\begin\\{longtable\\}', lscape_tbls)[[1]]

  # extract the position of the landscape environ declaration
  lsbegin_pos <- gregexpr('\\\\begin\\{landscape\\}', lscape_tbls)[[1]]

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
  ltend_pos <- gregexpr('\\\\end\\{longtable\\}', lscape_tbls)[[1]]

  # extract the position of the landscape environ ending
  lsend_pos <- gregexpr('\\\\end\\{landscape\\}', lscape_tbls)[[1]]

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
