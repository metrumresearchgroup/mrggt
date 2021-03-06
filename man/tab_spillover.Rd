% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab_create_modify.R
\name{tab_spillover}
\alias{tab_spillover}
\title{Add a spillover message for LaTeX table outputs}
\usage{
tab_spillover(
  data,
  message = NULL,
  message.align = c("right", "left", "center"),
  message.style = c("italic", "bold", "bold+italic"),
  repeat_column_labels = FALSE
)
}
\arguments{
\item{data}{A table object that is created using the \code{\link[=gt]{gt()}} function.}

\item{message}{message to display at \verb{\\endhead} (see \code{longtable} documentation)}

\item{message.align}{the alignment of the message. \code{left}, \code{right}, \code{center}, default is \code{right}}

\item{message.style}{styling for the message. \code{italic}, \code{bold}, \code{bold+italic}, default is \code{italic}}

\item{repeat_column_labels}{repeat the column labels + spanners for every page the table spans. default is \code{FALSE}}
}
\value{
An object of class \code{gt_tbl}.
}
\description{
Add a message that will appear at the bottom of every page that a table spans over until the table is completed.
}
\section{Figures}{

\if{html}{\figure{man_tab_spillover_latex_1.png}{options: width=100\%}}
\if{html}{\figure{man_tab_spillover_latex_2.png}{options: width=100\%}}
}

\examples{

# Create a very long table
fruit_tbl <-
  data.frame(
    rownm = rep(c(
      'fruit 1', 'fruit 2', 'fruit 3', 'fruit 4', 'fruit 5'
    ), 6),
    grpname = rep(c(
      'apple', 'banana', 'grape', 'pear', 'orange'
    ), 6),
    count = rep(c(1, 2, 3, 4, 5), 6),
    color = rep(c(
      'red', 'yellow', 'purple', 'green', 'orange'
    ), 6),
    stringsAsFactors = FALSE
  )

# add spanner, stubhead
# add footnotes, sourcenotes
# add spillover message to appear at bottom of table
# repeat the column labels/spanners on every page

fruit_tbl \%>\%
  gt(rowname_col = 'rownm') \%>\%
  tab_stubhead(label = 'Category') \%>\%
  tab_row_group(group = 'Repeating Group',
                rows = 1:5) \%>\%
  tab_spanner(label = 'Summary',
              columns = vars(grpname, count, color)) \%>\%
  summary_rows(columns = vars(count),
               groups = c('Repeating Group'),
               fns = list(Total = ~sum(.))) \%>\%
  tab_footnote(footnote = 'Total number present in set',
               locations = cells_column_labels('count')) \%>\%
  tab_source_note(c('Source: mrggt help guide')) \%>\%
  tab_options(source_notes.align = 'center') \%>\%
  tab_options(footnotes.align = 'left') \%>\%
  tab_spillover(message = 'Continued on Next Page...',
                message.align = 'right',
                message.style = 'italic',
                repeat_column_labels = TRUE) \%>\%
  as_latex()

}
