capture <- function() {
  # Get context
  rstudioapi::getActiveDocumentContext()
}

captureArea <- function(capture) {
  # Find range
  range_start <- capture$selection[[1L]]$range$start[[1L]]
  range_end   <- capture$selection[[1L]]$range$end[[1L]]

  # Stop if range is < 2 lines
  if ((range_end - range_start) < 1L) warning("At least two lines must be selected", .call = FALSE)

  # Dump contents and use highlighted lines as names.
  contents        <- capture$contents[range_start:range_end]
  names(contents) <- range_start:range_end
  return(contents)
}

findRegEx <- function(find, where, ...) {

  # Find and extract first match positions on each line (if any),
  positions    <- regexec(find, where, ...)
  matched_rows <- which(unlist(lapply(positions, `!=`, e2 = -1L)))
  positions    <- positions[matched_rows]

  # Stop if only one or fewer matches
  if (length(positions) <= 1L) warning("Did not match 2 or more regular expressions to align.")

  # Get range of highlighted lines, where matches were found,
  # and furthest position (to the right)
  highlighted_lines <- as.integer(names(where))
  matched_cols      <- vapply(positions, `[[`, integer(1L), 1L)
  which_max_col     <- which.max(matched_cols)
  # furthest_row    <- highlighted_lines[matched_rows[which_max_col]]
  furthest_column <- max(matched_cols)

  return(list(matched_rows      = matched_rows,
              matched_cols      = matched_cols,
              highlighted_lines = highlighted_lines,
              which_max_col     = which_max_col,
              furthest_column   = furthest_column))
}

assembleInsert <-function(info) {
  # Unload variables
  matched_rows      <- info$matched_rows
  matched_cols      <- info$matched_cols
  highlighted_lines <- info$highlighted_lines
  which_max_col     <- info$which_max_col
  furthest_column   <- info$furthest_column

  # Find the rows to align and the current column position of each regEx match.
  rows_to_align    <- highlighted_lines[matched_rows[-which_max_col]]
  columns_to_align <- matched_cols[-which_max_col]

  # Set location for spaces to be inserted.
  location <- Map(c, rows_to_align, columns_to_align)

  # Find and set the number of spaces to insert on each line.
  text.num <- furthest_column - columns_to_align
  text     <- vapply(text.num,
                     function(x) paste0(rep(" ", x), collapse = ""),
                     character(1L))

  return(list(location = location, text = text))
}

insertr <- function(list, id = NULL) {
  rstudioapi::insertText(list[["location"]], list[["text"]], id = id)
}
