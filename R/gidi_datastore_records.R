
#--------------------------
fields_classes <-
   NULL | class_list_numeric | class_list_character |
  S7::class_character | S7::class_numeric

check_fields <- S7::new_generic("check_fields","fields")
S7::method(check_fields,S7::class_any | fields_classes ) <-
  function(fields){fields}
S7::method(check_fields,S7::class_list) <- function(fields){
  if(can_be_list_of(fields,S7::class_character)){
    return(as_list_character(fields,check = FALSE))
  } else if (can_be_list_of(fields,S7::class_numeric)) {
    return(as_list_numeric(fields,check = FALSE))
  } else {
    return(fields)
  }
}

fields_setter <- function(self, value) {
    self@fields <- check_fields(value)
  self
}

fields_validator <- S7::new_generic("fields_validator","value",function(value) {
  S7::S7_dispatch()
})
S7::method(fields_validator,S7::class_any) <- function(value){
 "`fields` must be character, numeric, list of them, or NULL."
}
S7::method(fields_validator,fields_classes) <- function(value){
 NULL
}


#---------

limit_setter <- function(self, value) {
  value <- value %||% 32000
  if (!is.null(self@max_row)) {
    value <- min(value,self@max_row)
  }
  self@limit <-value
  self
  }

#-----------------
class_filters <-
  S7::new_class(
    "class_filters",
    S7::class_list,
    properties =
      list( atomic = prop_bool(default =quote(is_atomic_list(.data)) ))
    )


filters_validator <- function(value) {
  if (!is.null(value) && !is_a_or_list_of(value ,is_atomic_named_list) ) {
    "must be atomic named list, list of atomic named lists or NULL"
  }
}
filters_setter <- function(self, value) {
    self@filters <-
      if(is.list(value)){
        class_filters(value)
      } else {
        value
      }
  self
  }
compare_filters_fields <- function(filters_names, fields) {
  filters_names <- ensure_list(filters_names)
  character_fields <- is.character(fields)
  if (character_fields) {
    fields <-  list(fields)
  }
  filters_no_in_fields <-
    .mapply(
      Negate(all_in),
      list(filters_names, fields),
      list()
    ) |>
    collapse::vec()
  if (!any(filters_no_in_fields)) {
    list(ok = TRUE)
  } else if (length(filters_no_in_fields) == 1) {
    list(ok = FALSE ,msg =  "@filters names must be in @fields")
  } else {
    indx <- paste(which(filters_no_in_fields), collapse = ", ")
    if (character_fields) {
      msg <- paste0("@filters names not at [", indx, "] found in @fields")
    } else {
      msg <- paste0("@filters names not at found in @fields [", indx, "]")
    }
    list(ok = FALSE,msg = msg)
  }
}

#------------------------------
gidi_datastore_args <-
  S7::new_class(
  "gidi_datastore_args",
  properties =
    list(
      resource_id =
        S7::new_property(
          S7::class_character,
          setter = function(self, value) {
              self@resource_id <- check_resource_id(value)
              self
            }
        ),
      fields = S7::new_property(
        fields_classes,
        setter = fields_setter,
        validator = fields_validator),
      filters = S7::new_property(
        NULL | S7::class_list,
        setter = filters_setter,
        validator = filters_validator
        ),
      add_name = S7::new_property(
        class = S7::class_logical | S7::class_character,
        validator = function(value) {
          if (is.logical(value) && length(value) != 1) {
            "must be TRUE, FALSE or character vector"
          }
        },
        setter = function(self, value){
          if (rlang::is_named(self@resource_id) && isTRUE(value) ) {
            value <- names(self@resource_id)
          }
          self@add_name <-value
          self
        },
        default = TRUE
      ),
      fix_names = prop_bool(TRUE),
      limit = prop_number_whole(
        default = 32000,
        max = 32000,
        allow_null = TRUE,
        setter = limit_setter
      ),
      max_row = prop_number_whole(allow_null = TRUE)
    ),
  validator = function(self) {
    if (is.character(self@add_name) &&
      length(self@add_name) != length(self@resource_id)) {
      return("if @add_name is character is must be same length as @resource_id")
    }
    if (!is.null(self@filters)) {
      if (!self@filters@atomic) {
        if (length(self@filters) != length(self@resource_id)) {
          return("if @filters is nest list is must be same length as @resource_id")
        } else {
          filters_names <- lapply(self@filters, names)
        }
      } else {
        filters_names <- names(self@filters)
      }
      if (is.character(self@fields) || S7::S7_inherits(self@fields, class_list_character)) {
        comp <- compare_filters_fields(filters_names, self@fields)
        if (!comp$ok) {
          return(comp$msg)
        }
      }
    }
    return(NULL)
  }
)
#-------------------------




# validate_field_selection -----------------------
validate_field_selection  <-
  S7::new_generic("validate_field_selection", c("fields","col_names"))

S7::method(validate_field_selection ,list(NULL,S7::class_any)) <-
  function(fields,col_names){
    list(fields = fields, col_names = col_names)}

validate_numeric_fields <- function(fields, col_names) {
  n_col_names <-  collapse::vlengths(col_names)
  out_of_range <- n_col_names< collapse::fmax(fields)
  if (any(out_of_range)) {
    out_of_range_indx <- paste(which(out_of_range),collapse = ", ")
    stop(paste0("fields is out of range for resources [", out_of_range_indx, "]"))
  }
}
S7::method(validate_field_selection ,list(S7::class_numeric,S7::class_list)) <-
  function(fields,col_names) {
    validate_numeric_fields(fields, col_names)
      for (i in seq_along(col_names)) {
        col_names[[i]] <-   col_names[[i]][fields]
      }
      return(
        list(fields = class_list_character(col_names), col_names = col_names)
        )
  }
S7::method(validate_field_selection ,list(class_list_numeric,S7::class_list)) <-
  function(fields,col_names) {
      for (i in seq_along(col_names)) {
        col_names[[i]] <-   col_names[[i]][ fields[[i]] ]
      }
      return(list(fields = class_list_character(col_names), col_names = col_names))
  }

validate_character_fields <- function(names_not_found) {
  if (any(names_not_found)) {
    indx <- paste(which(names_not_found), collapse = ", ")
    stop(paste0(
      "field names not found in resources [",
      indx,
      "]"
    ))
  }
}
S7::method(validate_field_selection ,list(class_list_character,S7::class_list)) <-
  function(fields,col_names) {
names_not_found  <- vector(length = length(col_names))
  for (i in seq_along(col_names)) {
   if ( !all(fields[[i]] %in%  col_names[[i]]) ) {
     names_not_found [[i]] <- TRUE
   }
}
  validate_character_fields(names_not_found)
  return(list(fields = fields, col_names = fields))
  }
S7::method(validate_field_selection ,list(S7::class_character,S7::class_list)) <-
  function(fields,col_names) {
    names_not_found  <- vector(length = length(col_names))
    for (i in seq_along(col_names)) {
   if ( !all(fields %in%  col_names[[i]]) ) {
     names_not_found [[i]] <- TRUE
   }else {
     col_names[[i]] <- fields
   }
}
  validate_character_fields(names_not_found)
  return(list(fields = fields, col_names = col_names))
}

#-------------------------
gidi_datastore_base_info <-
  S7::new_class(
    "gidi_datastore_base_info",
    parent = gidi_datastore_args,
    properties =
      list(
        base_info =
          S7::new_property(
            S7::class_list,
            setter = function(self, value) {
              if (!is.null(self@base_info)) {
                return(self)
              }
              base_info <- get_resource_base_info(self@resource_id,filters = self@filters)
              self@base_info <- base_info
              S7::props(self) <-
                validate_field_selection(
                  self@fields,
                  base_info$col_names
                )
              self@n_rows <- resource_nrows(base_info, self@max_row)
              self
            }
          ),
        col_names =
          S7::new_property(
            NULL | S7::class_list,
            setter = function(self, value) {
              if (is.null(value)) {
                return(self)
              }

              if (self@fix_names &&
                can_be_list_of(value, S7::class_character)) {
                self@col_names <- Map(snakecase::to_snake_case, value)
              } else {
                self@col_names <- value
              }
              self
            }
          ),
        n_rows = NULL | S7::class_numeric,
        offset =
          S7::new_property(
            class_list_numeric,
            getter =
              function(self) {
                Map(seq_offset, limit = self@limit, total = self@n_rows) |>
                  class_list_numeric()
              }
          )
      )
  )




#---------------------------

gidi_datastore_records <-
  S7::new_class(
    "gidi_datastore_records",
    parent = gidi_datastore_base_info,
    properties =
      list(records =
             S7::new_property(
               S7::class_list,
               getter = function(self) {
                 records <- get_resource_records(
                   self@resource_id,
                   self@limit,
                   self@fields,
                   self@filters,
                   self@offset)
                 records <- Map(read_records,records,self@col_names)
                 records <- Map(collapse::qDT, records)
                 if (length(self@resource_id) == 1) {
                   records <- records[[1]]
                 } else if (is.character(self@add_name)) {
                   names(records) <- self@add_name
                 } else if (self@add_name) {
                   names(records) <- get_resource_name(self@resource_id)
                 }
                 return(records)
               } ))
      )





gidi_datastore2 <-
  function(resource_id,
           fields = NULL,
           filters = NULL,
           add_name = TRUE,
           fix_names = TRUE,
           limit = 32000,
           max_row = NULL) {
  gidi_datastore_records(
    resource_id = resource_id,
    fields = fields,
    filters = filters,
    add_name = add_name,
    fix_names = fix_names,
    limit = limit,
    max_row = max_row
  )@records
  }
