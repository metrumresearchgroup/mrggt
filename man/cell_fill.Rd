% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{cell_fill}
\alias{cell_fill}
\title{Helper for defining custom fills for table cells}
\usage{
cell_fill(color = "#D3D3D3", alpha = NULL)
}
\arguments{
\item{color}{The fill color. If nothing is provided, then \code{"#D3D3D3"} (light
gray) will be used as a default.}

\item{alpha}{An optional alpha transparency value for the \code{color} as single
value in the range of \code{0} (fully transparent) to \code{1} (fully opaque). If not
provided the fill color will either be fully opaque or use alpha
information from the color value if it is supplied in the #RRGGBBAA format.}
}
\value{
A list object of class \code{cell_styles}.
}
\description{
The \code{cell_fill()} helper function is to be used with the \code{\link[=tab_style]{tab_style()}}
function, which itself allows for the setting of custom styles to one or more
cells. Specifically, the call to \code{cell_fill()} should be bound to the
\code{styles} argument of \code{\link[=tab_style]{tab_style()}}.
}
\section{Figures}{

\if{html}{\figure{man_cell_fill_1.png}{options: width=100\%}}
}

\section{Function ID}{

7-16
}

\examples{
# Use `exibble` to create a gt table;
# add styles with `tab_style()` and
# the `cell_fill()` helper function
tab_1 <-
  exibble \%>\%
  dplyr::select(num, currency) \%>\%
  gt() \%>\%
  fmt_number(
    columns = vars(num, currency),
    decimals = 1
  ) \%>\%
  tab_style(
    style = cell_fill(color = "lightblue"),
    locations = cells_body(
      columns = vars(num),
      rows = num >= 5000)
  ) \%>\%
  tab_style(
    style = cell_fill(color = "gray85"),
    locations = cells_body(
      columns = vars(currency),
      rows = currency < 100
    )
  )

}
\seealso{
Other Helper Functions: 
\code{\link{adjust_luminance}()},
\code{\link{cell_borders}()},
\code{\link{cell_text}()},
\code{\link{cells_body}()},
\code{\link{cells_column_labels}()},
\code{\link{cells_column_spanners}()},
\code{\link{cells_grand_summary}()},
\code{\link{cells_row_groups}()},
\code{\link{cells_stubhead}()},
\code{\link{cells_stub}()},
\code{\link{cells_summary}()},
\code{\link{cells_title}()},
\code{\link{currency}()},
\code{\link{escape_latex}()},
\code{\link{gt_latex_dependencies}()},
\code{\link{html}()},
\code{\link{md}()},
\code{\link{pct}()},
\code{\link{px}()},
\code{\link{random_id}()}
}
\concept{Helper Functions}
