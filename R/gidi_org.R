#' @include gidi_obj_class.R

# Class Definitions ------------------------------------------------------------

#' Organization from data.gov.il
#'
#' Represents a government organization registered on the Israeli open data
#' portal. Every organization has a unique ID and bilingual names.
#'
#' @slot id Character. Unique organization identifier.
#' @slot eng_name Character. Organization name in English.
#' @slot heb_name Character. Organization name in Hebrew.
#'
#' @family gidi objects
#' @seealso [gidi_org_full_info] for the extended variant with counts,
#'   [list_gidi_org] for typed lists, [gidi_organization_list()] to fetch
#'   organizations from the API.
#' @export
gidi_org <- S7::new_class(
  "gidi_org",parent = gidi_obj,
  properties = list(
    id = S7::class_character,
    eng_name = S7::class_character,
    heb_name = S7::class_character
  )
)

#' Organization with Package and Resource Counts
#'
#' Extends [gidi_org] with the number of packages and total resources
#' published by the organization. Returned by [gidi_organization_list()] when
#' `names_only = FALSE`.
#'
#' @slot id Character. Unique organization identifier.
#' @slot eng_name Character. Organization name in English.
#' @slot heb_name Character. Organization name in Hebrew.
#' @slot packages Numeric. Number of data packages published.
#' @slot resources Numeric. Total number of resources across all packages.
#'
#' @family gidi objects
#' @export
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





#' Typed List of Organization Objects
#'
#' A list container that validates all elements are [gidi_org] (or subclass)
#' objects. Supports standard subsetting with `[` and `[[`.
#' Convert to a data frame with [as.data.frame()].
#'
#' @family gidi objects
#' @export
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

#' Convert a Named List to a gidi_org Object
#'
#' Inspects the names of `x` and returns either a [gidi_org] or a
#' [gidi_org_full_info] object depending on which fields are present.
#'
#' @param x A named list. Must contain either `id`, `eng_name`, `heb_name`
#'   (creates [gidi_org]) or those three plus `packages`, `resources`
#'   (creates [gidi_org_full_info]).
#' @return A [gidi_org] or [gidi_org_full_info] object.
#'
#' @examples
#' as_gidi_org(list(id = "123", eng_name = "Test Org", heb_name = "ארגון"))
#'
#' @family gidi objects
#' @export
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

