#' Table helpers for modelsummary()
#'
#' The \code{outputHelpers/msout} module provides helper functions for creating tables
#' with modelsummary::modelsummary().
'.__module__.'

#' Add means row
#' 
#' Create a dataframe appropriately formatted for adding a single row to
#' a modelsummary() object.
#' @param outcomes vector of strings or single string. variables for which means
#' are to be calculated. length must be 1 or the number of outcomes
#' in the table (use rep if 1 < len(outcomes) < no. Yvars)
#' @param titles vector of strings. must match the model titles in the table.
#' @param label string. The label for the row to be added to the table.
#' @param df Dataframe to calculate means
#' @return A tibble.
#' @export
add_means_row <- function(outcomes, titles, label, df){
  # calculate means
  means <-  sapply(outcomes, function(x) { round(mean(df[[x]], na.rm=T), 2) } )
  # reformat to get titles as col names
  data <- janitor::row_to_names(t(data.frame(titles, means)), 1)
  # add column at beginning as label
  data <- tibble::as_tibble(data)
  data <- dplyr::mutate(data, Coefficients = label)
  data <- dplyr::select(data, Coefficients, everything())
  return(data)
}

#' Add rows
#' 
#' Wrapper for the \code{add_means_row()} function for adding multiple rows
#' to a \code{modelsummary()} object.
#' @param rows list. Each entry represents an entry to the \code{outcomes}
#' field of \code{add_means_row()}; see this function description
#' for appropriate data types.
#' @param labels vector of strings. Each entry will be passed to the
#' \code{label} field of \code{add_means_row()}
#' @param titles vector of strings. Will be passed to the \code{titles} field
#'  of \code{add_means_row()}.
#' @param positions vector of integers, with \code{len(positions) = len(labels)}.
#'  Represents the row numbers of the rows to be added.
#' @param df dataframe from which means are calculated. This is passed to the
#'  \code{df} field of \code{add_means_row()}.
#' @return tibble.
#' @note An example of how to use this in conjunction with modelsummary() is as follows:
#' 
#' \code{modelsummary(regs, 
#' 
#' add_rows = regouthelpers$add_rows(list(c("Y1", "Y2"), "X"), c("Mean Y", "Mean X"), outcometitles, c(4,5), df))}
#' @export
add_rows <- function(rows, labels, titles, positions, df){
  # create a data from for each outcome list of interest
  rows <- purrr::map2(rows, labels, function(x, y){
    add_means_row(x, titles, y, df)
  })
  rows <- do.call(rbind.data.frame, rows)
  # add attribute for location in the output table
  attr(rows, 'position') <- positions 
  return(rows)
}
