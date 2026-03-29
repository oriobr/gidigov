#' @import S7

# ============================================================
# S7 Meta-Class Wrappers
# ============================================================
# S7 uses its own internal class types (S7_class, S7_union, etc.) which
# are implemented as S3 classes under the hood. To dispatch on these
# meta-types (e.g. "is this argument an S7 class? a union?") we wrap
# them as S7-compatible S3 class objects so they can appear in S7
# method signatures.

class_S7_union <-  S7::new_S3_class("S7_union")       # e.g. class_A | class_B
class_S7_class <-  S7::new_S3_class("S7_class")       # a concrete S7 class
class_S7_base_class <-  S7::new_S3_class("S7_base_class") # e.g. S7::class_numeric
class_S7_S3_class <-  S7::new_S3_class("S7_S3_class")     # an S3 class wrapped for S7

# Unions of the above -- used as method signatures to accept any class kind:
# "core" = base R types or S3 classes (not full S7 classes)
class_S7_core_class <- class_S7_S3_class|class_S7_base_class
# "any class" = S7 classes, core classes, or unions of them
class_S7_any_class <- class_S7_class|class_S7_core_class |class_S7_union
#------------------------

# Predicate helpers to test which kind of class descriptor a value is
is_S7_object <- function(x) {
  S7::S7_inherits(x)
}
is_S7_union <- function(x) {
  inherits(x, "S7_union")
}
is_S7_base_class <- function(x) {
  inherits(x, "S7_base_class")
}
is_S7_S3_class <- function(x) {
  inherits(x, "S7_S3_class")
}
is_S7_core_class <- function(x) {
  inherits(x, c("S7_S3_class","S7_base_class"))
}

# ============================================================
# class_checker -- polymorphic "is val an instance of class?"
# ============================================================
# Because the second argument `class` can be an S7 class, a base class,
# an S3 class, a union, or even a plain character string, we use S7
# generic dispatch on the *class descriptor* to pick the right check.
#---------------
#' Generic: class_checker
#'
#' Returns a function that checks if a value inherits
#' from the specified class.
class_checker <- S7::new_generic("class_checker",c("val", "class"))


#' Method for character class names
S7::method(class_checker,list(S7::class_any, S7::class_character)) <- function(val,class) {
  any(vapply(class, inherits, FUN.VALUE =logical(1), x = val))
}

#' Method for S7 objects
S7::method(class_checker,list(S7::class_any, class_S7_class)) <- function(val,class) {
  S7::S7_inherits(val, class)
}


#' Method for S7 core classes
S7::method(class_checker,list(S7::class_any, class_S7_base_class)) <- function(val,class) {
  typeof(val) == class[["class"]]
}

#' Method for S7 core classes
S7::method(class_checker,list(S7::class_any, class_S7_S3_class)) <- function(val,class) {
  inherits(val,  class[["class"]])
}

#' Method for S7 union classes
S7::method(class_checker,list(S7::class_any, class_S7_union)) <- function(val,class) {
  any(vapply(class[["classes"]], class_checker, FUN.VALUE =logical(1), val = val))
}
# ============================================================
# Helpers for checking inheritance against unions or class lists
# ============================================================

S7_inherits_union <- function(x,class) {
  any(
    vapply(
      class$classes,
      S7::S7_inherits,
      FUN.VALUE = logical(1), x = x)
  )
}

S7_inherits_class_list <- function(x,class_list) {
  any(
    vapply(
      class_list,
      S7::S7_inherits,
      FUN.VALUE = logical(1), x = x)
  )
}


# ============================================================
# check_S7_class -- guard that ensures an argument is a valid S7 class
# ============================================================

check_S7_class <- S7::new_generic("check_S7_any_class", "class")

S7::method(check_S7_class, class_S7_any_class) <- function(class) {
  class
}

S7::method(check_S7_class, S7::class_any) <- function(class) {
  stop("`class` must be an S7 class or S7 union.")
}
