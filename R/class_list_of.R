#' @include S7_utils.R

# ============================================================
# Base class definition for list-like S7 structures
# ============================================================

#' Base class: class_list_of
#'
#' A base class for representing lists whose elements
#' must all belong to a specified class.
class_list_of <- S7::new_class(
  name = "class_list_of",
  parent = S7::class_list
)

#' Print a `class_list_of` object
#'
#' @param x An object of class `class_list_of`.
#' @param ... Additional arguments passed to `print()`.
#' @export
S7::method(print, class_list_of) <- function(x, ...) {
  print(S7::S7_data(x), ...)
}

#' Subset operator for class_list_of
S7::method(`[`, class_list_of) <- function(object, ...) {
  S7::S7_data(object)[...]
}

#' Subset assignment operator for class_list_of
S7::method(`[<-`, class_list_of) <- function(object, ..., value) {
  S7::S7_data(object)[...] <- value
}

#' Extract operator for class_list_of
S7::method(`[[`, class_list_of) <- function(object, ...) {
  S7::S7_data(object)[[...]]
}

#' Extract assignment operator for class_list_of
S7::method(`[[<-`, class_list_of) <- function(object, ..., value) {
  S7::S7_data(object)[[...]] <- value
}




#-------------------


# ============================================================
# Factory: Create a new class_list_of subclass
# ============================================================

#' Create a new S7 class for lists of a specific class
#'
#' Defines an S7 class for a list whose elements inherit
#' from a given class. Includes validation for `NULL` values
#' and naming requirements.
#'
#' @param class An S7 class object, a base class, a union of classes,
#'   or a character string giving the name of a class.
#' @param allow_null Logical; whether `NULL` is allowed for the list.
#' @param names A character string indicating naming requirements for
#'   list elements. One of `"any"`, `"all"`, or `"none"`.
#'
#' @return An S7 class object with built-in validation rules.
new_class_list_of <- function(class, allow_null = FALSE, names = c("any", "all", "none")) {
  force(class)
  names <- rlang::arg_match(names)

  # Derive class name for messages
  class_name <- if (is_S7_object(class)) {
    class@name
  } else if (is_S7_core_class(class)) {
    class[["class"]]
  } else if (is_S7_union(class)) {
    class_name <- deparse(substitute(class))
    sub(".*class_(.+).*", "\\1", class_name)
  } else {
    class
  }

  # check_class <- build_class_check(class)
  class_list_name <- paste0("class_list_", class_name)

  force(class_name)
  force(class_list_name)

  S7::new_class(
    name = class_list_name,
    parent = if (allow_null) NULL | class_list_of else class_list_of,
    validator = function(self) {
      if (allow_null && is.null(self)) {
        return()
      }

      for (i in seq_along(self)) {
        val <- self[[i]]
        if (!class_checker(val,class)) {
          return(paste0(
            "must be a list of <",class_name,">s. ",
            "Element ", i, " is not."
          ))
        }
      }

      # Validate naming convention
      if (names == "all" && any(rlang::names2(self) == "")) {
        "must be a named list."
      } else if (names == "none" && any(rlang::names2(self) != "")) {
        "must be an unnamed list."
      }
    }
  )
}



# ============================================================
# Example concrete subclasses for common types
# ============================================================

#' A list of numeric elements
class_list_numeric <- new_class_list_of(S7::class_numeric)
as_list_numeric <- function(x, check = TRUE) {
  x <- collapse::copyMostAttrib(x,class_list_numeric())
  if (check) {
    S7::validate(x)
  }
  return(x)
}

#' A list of character elements
class_list_character <- new_class_list_of(S7::class_character)
as_list_character <- function(x, check = TRUE) {
  x <- collapse::copyMostAttrib(x,class_list_character())
  if (check) {
    S7::validate(x)
  }
  return(x)
}

#--------------

can_be_list_of <- S7::new_generic("can_be_list_of",c("x","class"))
S7::method(
  can_be_list_of,
  list(S7::class_list,
       S7::class_character |class_S7_any_class )) <-
  function(x,class){
         for (i in seq_along(x)) {
           val <- x[[i]]
           if (!class_checker(val,class)) {
             return(FALSE)
           }
         }
         return(TRUE)
       }
S7::method(
  can_be_list_of,
  list(S7::class_any,
       S7::class_character |class_S7_any_class )) <-
  function(x,class){ stop("'x' isn't a list") }

S7::method(can_be_list_of,list(S7::class_any,S7::class_any  )) <-
  function(x,class){ stop("'x' isn't a list and class must be S7 class or character") }

S7::method(can_be_list_of,list(S7::class_list,S7::class_list  )) <-
  function(x,class , any = TRUE){
    can_be <-  map_lgl(class,can_be_list_of,x = x )
      if (any) {
        return(any(can_be))
      } else {
        return(can_be)
      }

    }


