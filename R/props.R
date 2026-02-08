#' @include S7_utils.R

prop_string <- function(default = NULL, allow_null = FALSE, allow_na = FALSE,name = NULL, getter = NULL,
                        setter = NULL) {
  force(allow_null)
  force(allow_na)
  force(getter)
  force(setter)
  force(name)

  S7::new_property(
    name = name,
    class = if (allow_null) NULL | S7::class_character else S7::class_character,
    getter = getter,
    setter = setter,
    default = if (is.null(default) && !allow_null) {
      quote(stop("Required"))
    } else {
      default
    },
    validator = function(value) {
      if (allow_null && is.null(value)) {
        return()
      }

      if (length(value) != 1) {
        paste( "must be a single string")
      } else if (!allow_na && is.na(value)) {
        "must not be missing."
      }
    }
  )
}

prop_number_whole <- function(
    default = NULL,
    min = NULL,
    max = NULL,
    allow_null = FALSE,
    allow_na = FALSE,
    getter = NULL,
    setter = NULL
) {
  force(allow_null)
  force(allow_na)
  force(getter)
  force(setter)

  S7::new_property(
    class = if (allow_null) NULL | S7::class_numeric else S7::class_numeric,
    default = default,
    getter = getter,
    setter = setter,
    validator = function(value) {
      if (allow_null && is.null(value)) {
        return()
      }


      if (!rlang::is_scalar_integerish(value)) {
        "must be a whole number"
      } else if (!is.null(min) && value < min) {
        paste0("must be at least ", min, ", not ", value, ".")
      } else if (!is.null(max) && value > max) {
        paste0("must be at most ", max, ", not ", value, ".")
      } else if (!allow_na && is.na(value)) {
        "must not be missing."
      }
    }
  )
}


prop_bool <- function(default, allow_null = FALSE, allow_na = FALSE) {
  force(allow_null)
  force(allow_na)

  S7::new_property(
    class = if (allow_null) NULL | S7::class_logical else S7::class_logical,
    default = default,
    validator = function(value) {
      if (allow_null && is.null(value)) {
        return()
      }

      if (length(value) != 1) {
        if (allow_na) {
            "must be a single TRUE or FALSE"
        } else {
            "must be a single TRUE, FALSE or NA"
        }
      } else if (!allow_na && is.na(value)) {
        paste0("must be a TRUE or FALSE, not NA.")
      }
    }
  )
}

prop_enum <- function(values,
                      allow_null = FALSE,
                      default = if (allow_null) NULL else values[1],
                      exact = FALSE,
                      set_once = FALSE) {


  stopifnot(
    "values must be a character vector of length >= 2 without any NA" =
      is.character(values) && length(values) >= 2 && !anyNA(values)
  )


  display_values <-  paste0("'",values,"'",collapse = ",")
  message <- paste0("must be one of: ",display_values)
  force(values)
  force(message)
  force(allow_null)
  S7::new_property(
    class = if (allow_null) NULL | S7::class_character else S7::class_character,
    validator = function(value) {
      if (allow_null && is.null(value)) {
        return()
      }
        if (length(value) != 1L) {
          "must be length 1"
        } else if (!(value %in% values)) {
          message
        }
      },
    default = default
  )
}



prop_list_of <- function(class, allow_null = FALSE, names = c("any", "all", "none")) {
  force(class)
  names <- rlang::arg_match(names)
  class_name <-
    if (is_S7_object(class)) {
      class@name
    } else if (is_S7_core_class(class)) {
      class[["class"]]
    } else if (is_S7_union(class)) {
      class_name <- deparse(substitute(class))
      sub(".*class_(.+).*", "\\1", class_name)
    } else {
      class
    }

  check_class <- function(val) class_checker(val, class)
  force(class_name)
  force(check_class)

  S7::new_property(
    class = if (allow_null) {NULL | S7::class_list} else {S7::class_list},
    validator = function(value) {
      if (allow_null && is.null(value)) {
        return()
      }

      for (i in seq_along(value)) {
        val <- value[[i]]
        if (!check_class(val)) {
          return(paste0(
            "must be a list of <",
            class_name,
            ">s. ",
            "Element ",
            i,
            " is not."
          ))
        }
      }
      if (names == "all" && any(rlang::names2(value) == "")) {
        "must be a named list."
      } else if (names == "none" && any(rlang::names2(value) != "")) {
        "must be an unnamed list."
      }
    }
  )
}

prop_vector_or_list <-
  S7::new_generic("prop_vector_or_list", "class",
                   function(class, ...,
                                 allow_null = FALSE,
                                allow_na = FALSE,
                                names = c("any", "all", "none")) {
                    S7::S7_dispatch()
  }
  )
S7::method(prop_vector_or_list,class_S7_any_class) <- function(
    class,
    allow_null = FALSE,
    allow_na = FALSE,
    names = c("any", "all", "none")
) {

  force(class)
  force(allow_null)
  force(allow_na)
  names <- rlang::arg_match(names)

  class_name <-
    if (is_S7_object(class)) {
      class@name
    } else if (is_S7_core_class(class)) {
      class[["class"]]
    } else if (is_S7_union(class)) {
      class_name <- deparse(substitute(class))
      sub(".*class_(.+).*", "\\1", class_name)
    }
  force(class_name)

  S7::new_property(
    class =  if (allow_null) {NULL | S7::class_list |class } else {S7::class_list | class},
    validator = function(value) {
      if (allow_null && is.null(value)) {
        return()
      }

      if (is.list(value)) {
        for (i in seq_along(value)) {
          val <- value[[i]]
          if (!class_checker(val,class)) {
            paste0(
              "must be a <", class_name,"> vector or a list of <", class_name,">s. ",
              "Is a list, but element ", i, " is not a <", class_name,">."
            )
          }
        }
        if (names == "all" && any(rlang::names2(value) == "")) {
          "must be a named list."
        } else if (names == "none" && any(rlang::names2(value) != "")) {
          "must be an unnamed list."
        }
      } else {
        if (!class_checker(val,class)) {
          paste0(
            "must be a <",class_name,"> vector or a list of <", class_name,">s.",
            "Is a vector, but not a <", class_name,">."
          )
        }
        if (!allow_na && anyNA(value)) {
          "must not be missing."
        }

      }
    }
  )
}
