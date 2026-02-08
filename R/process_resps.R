#' Extract information from JSON responses
#'
#' @param resps List of httr2 responses
#' @param query Vector of JSON queries to extract specific paths
#' @param ... Additional parameters passed to RcppSimdJson::fparse
#' @return List of parsed response information
#' @noRd
extract_resps_info <- function(resps,
                               query = NULL,
                               ...,
                               empty_array = NA,
                               empty_object = NA,
                               single_null = NA) {
  resps_info <- lapply(ensure_list(resps), httr2::resp_body_raw) |>
    RcppSimdJson::fparse(
      query = query,
      empty_array = empty_array,
      empty_object = empty_object,
      single_null = single_null,
      ...
    )
  return(resps_info)
}

#' Extract specified fields from nested data structure
#'
#' @param x A nested data structure (list/data.frame)
#' @param names Vector of field names to extract
#' @return Data frame with extracted and flattened fields
#' @noRd
extract_fields <- function(x, names) {
  x <- rrapply::rrapply(x,
                        condition = function(x, .xname) {
                          .xname %in% names
                        },
                        how = "bind",
                        options = list(namesep = "_")
  )
  collapse::settransformv(x, is.character, trimws)
  return(x)
}

#' Select and rename variables in a data frame
#'
#' @param x Data frame to modify
#' @param vars Named vector of new variable names
#' @return Data frame with selected and renamed variables
#' @noRd
select_and_rename <- function(x, vars) {
  collapse::gv(x, vars, rename = TRUE)
}

#' Create a formula by swapping left and right hand sides
#'
#' @param lhs Left hand side variables
#' @param rhs Right hand side variables
#' @return Formula with swapped sides
#' @noRd
by_formula <- function(lhs, rhs) {
  frml <- reformulate(lhs, rhs)
  frml_lhs <- frml[[2]]
  frml_rhs <- frml[[3]]
  frml[[2]] <- frml_rhs
  frml[[3]] <- frml_lhs
  return(frml)
}

#' Nest data frame into a list structure based on grouping variables
#'
#' @param x Data frame to nest
#' @param by Grouping variables
#' @param FUN_convert Function to convert nested data frames
#' @param FUN_as_list Function to convert result to list
#' @param keep.by Logical indicating whether to keep grouping variables
#' @return Nested list structure
#' @noRd
gidi_nest_list <- function(x, by, FUN_convert, FUN_as_list, keep.by = FALSE) {
  x <- collapse::rsplit(x, by = by, keep.by = keep.by) |>
    rrapply::rrapply(
      f = FUN_convert,
      classes = "data.frame",
      how = "replace"
    )
  if(rlang::is_scalar_list(x) ){
    x <- x[[1]]
  } else {
    x <- FUN_as_list(x)
  }
  return(x)
}

#-----------------
