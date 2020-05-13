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
  expect_equal(scale_factors12[1:2], c(0.5, 0.3))

  # Expect a fixed value
  # scale factors for other columns should be equal to 1 (unchanged)
  expect_equal(tail(scale_factors12, -2), rep(1, length(scale_factors12) -2))

  # calculate scale factors for column widths to go from table 1 to table 3
  scale_factors13 <- lengths3/lengths

  # Expect a fixed value
  # scale factors for all columns should be equal to 0.4
  expect_equal(scale_factors13, rep(0.4, length(scale_factors13)))

  # Create a `tbl_latex` object with `gt()`;
  tbl_gt <-
    gt(data = mtcars_short) %>%
    as_latex()

  # Expect error
  # column 1 & 100 are scaled by 0.5 & 0.3
  # column 100 does not exist in table
  expect_error(tbl_gt %>%
                 latex_scale(scale_factor = list(c(1, 0.5),
                                               c(100, 0.3)))
  )

}
)
