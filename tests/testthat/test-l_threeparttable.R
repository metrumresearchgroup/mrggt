context("LaTeX -- Ensuring that three part table works as expected")

test_that("threeparttable environment", {

  tbl_fruit <- dplyr::tribble( ~grpname, ~count, ~color,
                               'apple', 1, 'red',
                               'banana', 2, 'yellow',
                               'grape', 3, 'purple',
                               'pear', 4, 'green',
                               'orange', 5, 'orange')

  # Create a `tbl_latex` object with `gt()`;
  tbl_gt <-
    gt(data = tbl_fruit) %>%
    as_latex() %>%
    as.character()

  # split the character str by `\n`
  latex_vec <- unlist(strsplit(tbl_gt, '\n'))

  # Expect a fixed value
  # first line should be declaration of threeparttable
  expect_equal('\\begin{ThreePartTable}', latex_vec[1])

  # find where caption set up is declared
  cap_setup <- latex_vec[startsWith(latex_vec, "\\captionsetup")]

  # Expect a fixed pattern
  # caption should be left aligned (ragged right)
  expect_true(grepl('justification=raggedright', cap_setup))

  # Expect a fixed value
  # last line should be end of threeparttable environment
  expect_equal('\\end{ThreePartTable}', latex_vec[length(latex_vec)])
})
