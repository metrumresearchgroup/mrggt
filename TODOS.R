# TODO: inspect all build_data initializer functions (e.g., `migrate_unformatted_to_output()`)
  # TODO: make all functions dt methods
  # TODO: make functions that modify footnotes/styles into dt methods
# TODO: fix rtf rendering

# TODO:
# cells_border fix
# convert style_latex to s3 method
# tests for sourcenotes.align & footnotes.align
# tests:
   # tab_options(footnotes.align = ) X
   # tab_options(sourcenotes.align = )
   # tab_options(table.optimize.font = )
   # tab_options(table.optimize.width =)
   # tab_caption()
   # grand_summary_rows()
     # ensure proper placement (last row of table)
     # ensure any merge patterns present in columns are also present in summary
     # ensure proper latex
   # summary_rows
     # ensure proper placement (if summarizing row 1-3, should be inserted at row 4)
     # ensure merge patterns
     # proper latex
     # if stub is not present, a new empty column at start of table is created and summary label goes there
     # if stub present, summary label goes in stub column

