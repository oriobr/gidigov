#' Base Classes for gidi Objects
#'
#' @description
#' Abstract base classes for the gidigov object hierarchy:
#' - [gidi_obj]: Abstract base for individual entities (organizations,
#'   packages, resources).
#' - [gidi_objs_list]: Abstract base for typed lists of gidi objects.
#'
#' All gidi objects support [as.data.frame()] and pretty-print methods.
#'
#' @name gidi_obj_class
#' @family gidi objects
NULL

#' Abstract Base Class for gidi Objects
#'
#' All concrete gidi classes ([gidi_org], [gidi_pak], [gidi_resource])
#' inherit from this abstract class. It provides shared behaviour such as
#' [as.data.frame()] conversion.
#'
#' @family gidi objects
#' @export
gidi_obj <- S7::new_class("gidi_obj",abstract = TRUE)

#' Abstract Base Class for Lists of gidi Objects
#'
#' Typed list container for gidi objects. Concrete list classes
#' ([list_gidi_org], [list_gidi_pak], [list_gidi_resource]) are created
#' via [new_gidi_objs_list()]. Supports subsetting with `[` and `[[`
#' and conversion with [as.data.frame()].
#'
#' @family gidi objects
#' @export
gidi_objs_list <- S7::new_class("gidi_objs_list", class_list_of)


S7::method(`[`, gidi_objs_list) <- function(object, ...) {
  collapse::copyMostAttrib(S7::S7_data(object)[...],object)
}

#' Create a new gidi objects list class
#'
#' @description
#' Factory function to create a new class for lists of specific gidi objects.
#' The new class inherits from gidi_objs_list and includes validation to ensure
#' all elements are of the specified gidi object class.
#'
#' @param gidi_obj_class The gidi object class that elements of the list must inherit from
#' @return A new S7 class for lists of the specified gidi object type
#' @export
#'
#' @examples
#' # Create a new class for lists of gidi_pak objects
#' list_gidi_pak <- new_gidi_objs_list(gidi_pak)
new_gidi_objs_list <- function(gidi_obj_class) {
  force(gidi_obj_class)
  class_name <- deparse(substitute(gidi_obj_class))
  force(class_name)
  class_list_name <- paste0("list_", class_name)
  force(class_list_name)

  S7::new_class(
    class_list_name,
    parent = gidi_objs_list,
    validator = function(self) {
      not_are_gidi_obj <- rapply(self, \(x) {
        !S7::S7_inherits(x, gidi_obj_class)
      })
      if (any(not_are_gidi_obj)) {
        return(paste0("must be a list of <", class_name, ">s. "))
      }
    }
  )
}

#' Print method for gidi_objs_list
#'
#' @param x A gidi_objs_list object
#' @param ... Additional arguments passed to print
#' @export
S7::method(print, gidi_objs_list) <- function(x, ...) {
  print(S7::S7_data(x))
}

#' Convert gidi Objects to Data Frames
#'
#' S7 methods for [as.data.frame()] that convert a single gidi object or a
#' list of gidi objects into a flat data frame (one row per object).
#'
#' @name as.data.frame.gidi
#' @param x A [gidi_obj] or [gidi_objs_list] object.
#' @param ... Ignored.
#' @return A data.frame. For [gidi_objs_list], rows are bound with
#'   `collapse::rowbind(fill = TRUE)` so that different subclasses (e.g.,
#'   [gidi_org] mixed with [gidi_org_full_info]) are handled gracefully.
#'
#' @family gidi objects
#' @export
NULL

#' @rdname as.data.frame.gidi
S7::method(as.data.frame, gidi_obj) <- function(x, ...) {
  data.frame(S7::props(x))
}

#' @rdname as.data.frame.gidi
S7::method(as.data.frame, gidi_objs_list) <- function(x, ...) {
  Map(as.data.frame, x) |>
    collapse::rowbind(fill = TRUE)
}
