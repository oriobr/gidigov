#' Base classes for gidi objects
#'
#' @description
#' This file defines the base classes and methods for gidi objects:
#' - gidi_obj: Base class for individual gidi objects
#' - gidi_objs_list: Base class for lists of gidi objects
#'
#' @name gidi_obj_class
NULL

#' Base class for gidi objects
#' @export
gidi_obj <- S7::new_class("gidi_obj",abstract = TRUE)

#' Base class for lists of gidi objects
#' @export
# gidi_objs_list <- S7::new_class("gidi_objs_list", S7::class_list)
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

#' Convert gidi objects to data frames
#'
#' @name as.data.frame.gidi
#' @param x A gidi_obj or gidi_objs_list object
#' @param ... Additional arguments passed to as.data.frame
#' @return A data frame representation of the object
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
