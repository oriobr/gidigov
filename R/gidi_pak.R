#' @include gidi_obj_class.R


# Class Definition ------------------------------------------------------------

#' Data Package from data.gov.il
#'
#' Represents a dataset package on the Israeli open data portal. Each package
#' belongs to an organization and may contain multiple resources.
#'
#' @slot pak_id Character. Unique package identifier.
#' @slot pak_heb_name Character. Package name in Hebrew.
#' @slot pak_eng_name Character. Package name in English (URL slug).
#' @slot org_heb_name Character. Parent organization name in Hebrew.
#' @slot org_eng_name Character. Parent organization name in English.
#'
#' @family gidi objects
#' @seealso [list_gidi_pak] for typed lists, [gidi_paks_by_org()] and
#'   [gidi_search_pak()] to retrieve packages from the API.
#' @export
gidi_pak <- S7::new_class(
  "gidi_pak",
  parent = gidi_obj,
  properties = list(
    pak_id = S7::class_character,
    pak_heb_name = S7::class_character,
    pak_eng_name = S7::class_character,
    org_heb_name = S7::class_character,
    org_eng_name = S7::class_character
  )
)

is_gidi_pak <- function(x) {
  S7::S7_inherits(x,gidi_pak)
}


#' Typed List of Package Objects
#'
#' A list container that validates all elements are [gidi_pak] objects.
#' Supports standard subsetting with `[` and `[[`.
#' Convert to a data frame with [as.data.frame()].
#'
#' @family gidi objects
#' @export
list_gidi_pak <- new_gidi_objs_list(gidi_pak)


# Print Methods --------------------------------------------------------------

#' Print method for pak
#' @param x A gidi_pak object
#' @param ... Additional arguments passed to print
S7::method(print, gidi_pak) <- function(x, ...) {
  cli::cli_text("{.cls gidi_pak}")
  cli::cli_text("{.strong Organization Name:}")
  cli::cli_text("{.field English:} {x@org_eng_name}")
  cli::cli_text("{.field Hebrew:} {x@org_heb_name}")
  cli::cli_text("{.strong Package Name:}")
  cli::cli_text("{.field English:} {x@pak_eng_name}")
  cli::cli_text("{.field Hebrew:} {x@pak_heb_name}")
  cli::cli_text("{.field Package ID:} {x@pak_id}")
  invisible(x)
}

#' #' Print method for pak list
#' #' @param x A list_gidi_pak object
#' #' @param ... Additional arguments passed to print
#' S7::method(print, list_gidi_pak) <- function(x, ...) {
#'   print(S7::S7_data(x))
#' }


# Constructor Function ------------------------------------------------------

#' Convert a Named List to a gidi_pak Object
#'
#' @param x A named list with fields `pak_id`, `pak_heb_name`,
#'   `pak_eng_name`, `org_heb_name`, and `org_eng_name`.
#' @return A [gidi_pak] object.
#'
#' @examples
#' as_gidi_pak(list(
#'   pak_id = "12345",
#'   pak_heb_name = "חבילה",
#'   pak_eng_name = "example-package",
#'   org_heb_name = "ארגון",
#'   org_eng_name = "example-org"
#' ))
#'
#' @family gidi objects
#' @export
as_gidi_pak <- function(x) {
  # Input validation
  stopifnot(
    "'x' must be a list" = is.list(x),
    "'x' must be named" = rlang::is_named(x),
    "'x' cannot contain NULL values" = !any(vapply(x, is.null, logical(1)))
  )

  do.call(gidi_pak, x)
}

