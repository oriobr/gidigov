# # https://josiahparry.com/posts/2023-11-10-enums-in-r/index.html#making-an-s7-enum-object-in-r
#
#
#
#
#
#
# # create a new Enum abstract class
# Enum <- new_class(
#   "Enum",
#   properties = list(
#     Value = class_character,
#     Variants = class_character
#   ),
#   validator = function(self) {
#     if (length(self@Value) != 1L) {
#       "enum value's are length 1"
#     } else if (!(self@Value %in% self@Variants)) {
#       "enum value must be one of possible variants"
#     }
#   },
#   abstract = TRUE
# )
#
#
# # create a new enum constructor
# new_enum_class <- function(enum_class, variants) {
#   new_class(
#     enum_class,
#     parent = Enum,
#     properties = list(
#       Value = class_character,
#       Variants = new_property(class_character, default = variants)
#     ),
#     constructor = function(Value) {
#       new_object(S7_object(), Value = Value, Variants = variants)
#     }
#   )
# }
# # create a new Enum abstract class
# Enum <- S7::new_class(
#   "Enum",S7::class_character,
#   properties = list(
#     Value = class_character,
#     Variants = class_character
#   ),
#   validator = function(self) {
#     if (length(self@Value) != 1L) {
#       "enum value's are length 1"
#     } else if (!(self@Value %in% self@Variants)) {
#       paste0("enum value must be one of: ",toString(self@Variants))
#     }
#   }
# )
#
# # create a new enum constructor
# new_enum_class <- function(enum_class, variants) {
#   new_class(
#     enum_class,
#     parent = Enum,
#     properties = list(
#       Variants = new_property(class_character, default = variants),
#       Value = new_property(
#         class_character,
#         getter = function(self) {S7_data(self)})
#     ),
#     # constructor = function(x) {
#     #   new_object(x, Value =x,Variants = variants)
#     # }
#   )
# }
#
# .records_format <- new_enum_class(".records_format" ,c("objects", "lists", "csv", "tsv"))
#
#
# .records_format( "ee")
#
# .records_format("csv")
#
# class_character |> dput()
#
#
# structure(
#   list(
#     class = "character",
#     constructor_name = "character",
#     constructor = function (.data = character(0))
#       .data,
#     validator = function (object)
#     {
#       if (base_class(object) != name) {
#         sprintf("Underlying data must be <%s> not <%s>",
#                 name,
#                 base_class(object))
#       }
#     }
#   ),
#   class = "S7_base_class"
# )
