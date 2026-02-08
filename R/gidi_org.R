#' @include gidi_obj_class.R

# Class Definitions ------------------------------------------------------------

#' Base class for organization representation
#' @description Creates a new organization class with basic information
#' @note All fields are required and must be character strings
gidi_org <- S7::new_class(
  "gidi_org",parent = gidi_obj,
  properties = list(
    id = S7::class_character,
    eng_name = S7::class_character,
    heb_name = S7::class_character
  )
)

#' Extended organization class with additional metrics
#' @description Extends gidi_org with package and resource information
#' @note Inherits all properties from gidi_org
gidi_org_full_info <- S7::new_class(
  "gidi_org_full_info",
  parent = gidi_org,
  properties = list(
    packages = S7::class_numeric,
    resources = S7::class_numeric
  )
)

is_gidi_org <- function(x) {
  S7::S7_inherits(x,gidi_org)
}





#' List class for holding multiple organizations
#' @description Creates a list class for managing multiple organization objects
list_gidi_org <- new_gidi_objs_list(gidi_org)
is_list_gidi_org <- function(x) {
  S7::S7_inherits(x,list_gidi_org)
}

# Print Methods --------------------------------------------------------------

#' Print method for basic organization
#' @param x A gidi_org object
#' @param ... Additional arguments passed to print
S7::method(print, gidi_org) <- function(x, ...) {
  cli::cli_text("{.cls gidi_org}")
  cli::cli_text("{.strong Organization:}")
  cli::cli_text("{.field English Name:} {x@eng_name}")
  cli::cli_text("{.field Hebrew Name:} {x@heb_name}")
  cli::cli_text("{.field ID:} {x@id}")
  invisible(x)
}

#' Print method for full organization info
#' @param x A gidi_org_full_info object
#' @param ... Additional arguments passed to print
S7::method(print, gidi_org_full_info) <- function(x, ...) {
  cli::cli_text("{.cls gidi_org}")
  cli::cli_text("{.strong Organization:}")
  cli::cli_text("{.field English Name:} {x@eng_name}")
  cli::cli_text("{.field Hebrew Name:} {x@heb_name}")
  cli::cli_text("{.field Packages:} {x@packages}")
  cli::cli_text("{.field Resources:} {x@resources}")
  cli::cli_text("{.field ID:} {x@id}")
  invisible(x)
}


# Constructor Function ------------------------------------------------------


has_exact_names <- function(x_names, expected_names) {
  length(x_names) == length(expected_names) &&
    all(x_names %in% expected_names)
}

#' Create a new organization object
#' @param x A named list containing organization data
#' @return A gidi_org or gidi_org_full_info object
#' @throws Error if input is not properly named or has invalid structure
#' @examples
#' org_data <- list(
#'   id = "123",
#'   eng_name = "Test Org",
#'   heb_name = "ארגון בדיקה"
#' )
#' as_gidi_org(org_data)
as_gidi_org <- function(x) {
  # Input validation
  stopifnot("'x' must be named" = rlang::is_named(x))

  # Define expected field names
  x_names <- names(x)
  gidi_org_names <- c("id", "eng_name", "heb_name")
  gidi_org_full_info_names <- c("id", "eng_name", "heb_name", "packages", "resources")

  # Create appropriate object based on input structure
  if (has_exact_names(x_names,gidi_org_full_info_names) ) {
    return(do.call(gidi_org_full_info, x))
  } else if (has_exact_names(x_names,gidi_org_names)) {
    return(do.call(gidi_org, x))
  }

  stop("Invalid input structure. Must match either gidi_org or gidi_org_full_info fields.")
}

