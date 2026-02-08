#' @include gidi_obj_class.R


# Class Definition ------------------------------------------------------------

#' Base class for pak representation
#' @description Creates a new pak class for the Israeli government data repository
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


#' List class for holding multiple paks
#' @description Creates a list class for managing multiple pak objects
#'
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

#' Create a new pak object
#' @param x A named list containing pak data
#' @return A gidi_pak object
#' @examples
#' pak_data <- list(
#'   pak_id = "12345",
#'   pak_heb_name = "חבילה לדוגמה",
#'   pak_eng_name = "Example Package",
#'   org_heb_name = "ארגון לדוגמה",
#'   org_eng_name = "Example Organization"
#' )
#' as_gidi_pak(pak_data)
as_gidi_pak <- function(x) {
  # Input validation
  stopifnot(
    "'x' must be a list" = is.list(x),
    "'x' must be named" = rlang::is_named(x),
    "'x' cannot contain NULL values" = !any(vapply(x, is.null, logical(1)))
  )

  do.call(gidi_pak, x)
}

