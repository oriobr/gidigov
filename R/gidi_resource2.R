#' @include gidi_obj_class.R


# Class Definition ------------------------------------------------------------

class_date <- S7::class_Date | S7::class_POSIXct | S7::class_POSIXlt


#' Base class for resource representation
#' @description Creates a new resource class for the Israeli government data repository
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





#' List class for holding multiple resources
#' @description Creates a list class for managing multiple resource objects
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

#' Create a new resource object
#' @param x A named list containing resource data
#' @return A gidi_resource object
#' @examples
#' resource_data <- list(
#'   resource_id = "56789",
#'   pak_heb_name = "חבילה לדוגמה",
#'   pak_eng_name = "Example Package",
#'   resource_heb_name = "משאב לדוגמה",
#'   size = 1024,
#'   last_modified = Sys.time(),
#'   created = Sys.time()
#' )
#' as_gidi_resource(resource_data)
as_gidi_resource <- function(x) {
  do.call(gidi_resource, x)
}









