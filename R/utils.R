#' Ensure input is a list
#'
#' Converts input to a list if it's not already a list or if it contains atomic elements.
#' This function is useful for ensuring consistent list structure in data processing.
#'
#' @param x An object to convert to list if needed
#' @return A list containing x, or x itself if already a list and contains no atomic elements
#' @noRd
ensure_list <- function(x) {
  if (!is.list(x) ||
      collapse::has_elem(x,is.atomic,recursive = FALSE)) {
    list(x)
  } else {
    x
  }
}
#-------------------------------------
## https://github.com/t-kalinowski/quickr/blob/main/R/aaa-utils.R
map_int <- function(.x, .f, ...) vapply(X = .x, FUN = .f, FUN.VALUE = 0L, ...)
map_lgl <- function(.x, .f, ...) vapply(X = .x, FUN = .f, FUN.VALUE = TRUE, ...)
map_chr <- function(.x, .f, ...) vapply(X = .x, FUN = .f, FUN.VALUE = "", ...)


#-------------------------------------
is_atomic_list <- function(x) {
  is.list(x) &&  !collapse::has_elem(x,is.recursive,recursive = FALSE)
}

is_atomic_named_list <- function(x) {
  is_atomic_list(x) && rlang::is_named(x)
}


is_a_or_list_of <- function(x, checker) {
  checker(x) || all(map_int(x, checker))
}



all_in <- function(L, R) {
  all(L %in% R )
}


#' Null coalescing operator
#'
#' Returns y if x is NULL, otherwise returns x.
#' Similar to %||% but with reversed logic.
#'
#' @param x First value to check
#' @param y Value to return if x is NULL
#' @return x if x is not NULL, otherwise y
#' @examples
#' NULL %|!|% 1     # Returns 1
#' "a" %|!|% "b"    # Returns "b"
#' list() %|!|% 1   # Returns list()
#' @noRd
`%|!|%` <- function(x, y) {
  if (is.null(x)) x else y
}

#' Conditional if operator
#'
#' Returns the prior value if the proposition is TRUE, otherwise returns NULL.
#' Useful for conditional execution in a pipeline.
#'
#' @param prior The value to potentially return
#' @param proposition Logical condition to evaluate
#' @return prior if proposition is TRUE, NULL otherwise
#' @examples
#' 5 %if% TRUE      # Returns 5
#' 5 %if% FALSE     # Returns NULL
#' @noRd
`%if%` <- function(prior, proposition) {
  if (proposition) return(prior)
}

#' Conditional unless operator
#'
#' Returns the prior value if the proposition is FALSE, otherwise returns NULL.
#' Acts as the inverse of the %if% operator.
#'
#' @param prior The value to potentially return
#' @param proposition Logical condition to evaluate
#' @return prior if proposition is FALSE, NULL otherwise
#' @examples
#' 5 %unless% TRUE      # Returns NULL
#' 5 %unless% FALSE     # Returns 5
#' @noRd
`%unless%` <- function(prior, proposition) {
  if (!proposition) return(prior)
}

#' Check if object is an httr2 request
#'
#' Tests whether an object inherits from the httr2_request class.
#' Useful for validating inputs in functions that work with HTTP requests.
#'
#' @param x Object to test
#' @return Logical indicating whether x is an httr2 request
#' @examples
#' req <- httr2::request("https://example.com")
#' is_request(req)     # Returns TRUE
#' is_request("test")  # Returns FALSE
#' @noRd
is_request <- function(x) {
  inherits(x, "httr2_request")
}



list_length_match <- function(x, y) {
  !is.list(x) ||length(y) == length(x)
}

## https://github.com/t-kalinowski/quickr/blob/main/R/aaa-utils.R
is_scalar <- function(x) identical(length(x), 1L)

is_scalar_na      <- function(x) is.atomic(x)    && !is.object(x) && length(x) == 1L && is.na(x)
is_scalar_atomic  <- function(x) is.atomic(x)    && !is.object(x) && length(x) == 1L && !is.na(x)
is_scalar_integer <- function(x) is.integer(x)   && !is.object(x) && length(x) == 1L && !is.na(x)
is_string         <- function(x) is.character(x) && length(x) == 1L && !is.na(x) # could also be 'glue' class.
is_bool           <- function(x) is.logical(x)   && !is.object(x) && length(x) == 1L && !is.na(x)
is_number         <- function(x) is.numeric(x)   && !is.object(x) && length(x) == 1L && !is.na(x)
is_wholenumber    <- function(x) is.numeric(x)   && !is.object(x) && length(x) == 1L && !is.na(x) &&
  x >= 0L && (is.integer(x) || is.double(x) && trunc(x) == x)
