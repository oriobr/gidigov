#' @include gidi_obj_class.R


# Class Definition ------------------------------------------------------------

class_date <- S7::class_Date | S7::class_POSIXct | S7::class_POSIXlt


#' Data Resource from data.gov.il
#'
#' Represents a single data resource (e.g., a CSV file or API endpoint)
#' within a package on the Israeli open data portal.
#'
#' @slot resource_id Character. Unique resource identifier.
#' @slot pak_heb_name Character. Parent package name in Hebrew.
#' @slot pak_eng_name Character. Parent package name in English.
#' @slot resource_heb_name Character. Resource name in Hebrew.
#' @slot size Numeric. File size in bytes.
#' @slot last_modified POSIXct/Date. When the resource was last updated.
#' @slot created POSIXct/Date. When the resource was created.
#' @slot see Function. Call `resource@see()` to open the resource page in the
#'   browser (read-only computed property).
#'
#' @family gidi objects
#' @seealso [list_gidi_resource] for typed lists,
#'   [gidi_resources_by_pak()] to retrieve resources from the API,
#'   [gidi_datastore()] to download the resource's data.
#' @export
gidi_resource <- S7::new_class(
  "gidi_resource",
  parent = gidi_obj,
  properties = list(
    resource_id = S7::class_character,
    pak_heb_name = S7::class_character,
    pak_eng_name = S7::class_character,
    resource_heb_name = S7::class_character,
    size = S7::class_numeric,
    last_modified = class_date,
    created = class_date,
    see = S7::new_property(
      S7::class_function,
      getter = function(self) {function(){gidi_see(self)}})
  )
)


is_gidi_resource <- function(x) {
  S7::S7_inherits(x,gidi_resource)
}





#' Typed List of Resource Objects
#'
#' A list container that validates all elements are [gidi_resource] objects.
#' Supports standard subsetting with `[` and `[[`.
#' Convert to a data frame with [as.data.frame()].
#'
#' @family gidi objects
#' @export
list_gidi_resource <- new_gidi_objs_list(gidi_resource)

is_list_gidi_resource <- function(x) {
  S7::S7_inherits(x,list_gidi_resource)
}




# Print Methods --------------------------------------------------------------

#' Print method for resource
#' @param x A gidi_resource object
#' @param ... Additional arguments passed to print
S7::method(print, gidi_resource) <- function(x, ...) {
  cli::cli_text("{.cls gidi_resource}")
  cli::cli_text("{.strong Package Name:}")
  cli::cli_text("{.field English:} {x@pak_eng_name}")
  cli::cli_text("{.field Hebrew:} {x@pak_heb_name}")
  cli::cli_text("{.strong Resource:}")
  cli::cli_text("{.field Name:} {x@resource_heb_name}")
  cli::cli_text("{.field Size:} {prettyunits::pretty_bytes(x@size)}")
  cli::cli_text("{.field ID:} {x@resource_id}")
  cli::cli_text("{.field Created:} {format(x@created)}")
  cli::cli_text("{.field Last Modified:} {format(x@last_modified)}")
  invisible(x)
}


# Conversion Methods --------------------------------------------------------

# Constructor Function ------------------------------------------------------

#' Convert a Named List to a gidi_resource Object
#'
#' @param x A named list with fields `resource_id`, `pak_heb_name`,
#'   `pak_eng_name`, `resource_heb_name`, `size`, `last_modified`, and
#'   `created`.
#' @return A [gidi_resource] object.
#'
#' @family gidi objects
#' @export
as_gidi_resource <- function(x) {
  do.call(gidi_resource, x)
}









