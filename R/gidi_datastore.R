#' @include gidi_resource2.R
#' @include utils.R
#' @include get_resps_datastore_search.R


#' #' Create datastore search request
#' #'
#' #' @param resource_id Character string of resource ID
#' #' @param records_format Format of records (objects, lists, csv, or tsv)
#' #' @param limit Maximum number of records to return
#' #' @param fields Character vector of field names to return
#' #' @param include_total Logical; whether to include total count in response
#' #' @param row_offset Integer vector of row offsets for pagination
#' #' @return A list of httr2 request objects
#' #' @noRd
#' get_req_datastore_search_ <-
#'   function(resource_id,
#'            records_format = NULL,
#'            limit = NULL,
#'            fields = NULL,
#'            include_total = NULL,
#'            row_offset = NULL) {
#'     req <-
#'       data_gov_api_req("datastore_search") |>
#'       httr2::req_url_query(
#'         resource_id = resource_id,
#'         records_format = records_format,
#'         limit = limit,
#'         fields = fields,
#'         include_total = include_total ,
#'         .multi = "comma"
#'       )
#'     if (is.null(row_offset)) {
#'       return(list(req))
#'     }
#'
#'     reqs <-
#'       .mapply(
#'         httr2::req_url_query,
#'         dots = list("offset" = collapse::vec(row_offset)),
#'         MoreArgs = list(.req = req)
#'       )
#'     return(reqs)
#'   }
#' #' Create multiple datastore search requests
#' #'
#' #' @param resource_id Character vector of resource IDs
#' #' @param records_format Format of records (objects, lists, csv, or tsv)
#' #' @param limit Maximum number of records to return
#' #' @param fields Character vector or list of field names to return
#' #' @param include_total Logical; whether to include total count in response
#' #' @param row_offset Integer or list of row offsets for pagination
#' #' @param ... Additional arguments passed to internal functions
#' #' @return A list of httr2 request objects
#' #' @noRd
#' get_req_datastore_search <- function(resource_id,
#'                                      records_format = NULL,
#'                                      limit = NULL,
#'                                      fields = NULL,
#'                                      include_total = NULL,
#'                                      row_offset = NULL,
#'                                      ...) {
#'   records_format <- match.arg(records_format, c("objects", "lists", "csv", "tsv"))
#'   stopifnot(
#'     "'resource_id=' must be a character" = collapse::allv(collapse::vclasses(fields), "character"),
#'     "'fields=' is a list (nested fields), but is not the same length as 'resource_id='" = list_length_match(fields,resource_id),
#'     "'offset=' is a list (nested fields), but is not the same length as 'resource_id='" = list_length_match(row_offset,resource_id),
#'     "'limit=' must be 'NULL' or a single integerish" = is.null(limit) ||
#'       rlang::is_scalar_integerish(limit)
#'   )
#'
#'   dots <- list(resource_id = resource_id)
#'   MoreArgs <-
#'     list(limit = limit %|!|% as.integer(limit),
#'          records_format = records_format,
#'          include_total = include_total)
#'
#'
#'   if (!is.null(fields)) {
#'     stopifnot("'fields=' must be 'NULL' or a character" = all(collapse::vclasses(fields) %in% c("NULL", "character")))
#'     if (is.list(fields)) {
#'       dots$fields <- fields
#'     } else {
#'       MoreArgs$fields <- fields
#'     }
#'   }
#'
#'   if (!is.null(row_offset)) {
#'     if (is.list(row_offset)) {
#'       dots$row_offset <- row_offset
#'     } else {
#'       MoreArgs$row_offset <- row_offset
#'     }
#'   }
#'
#'   return(
#'   .mapply(get_req_datastore_search_,dots= dots,MoreArgs= MoreArgs) |>
#'     collapse::vec()
#'     )
#'
#' }
#'
#' #' Perform parallel datastore search requests
#' #'
#' #' @param resource_id Character vector of resource IDs
#' #' @param records_format Format of records (default: "csv")
#' #' @param ... Additional parameters passed to get_req_datastore_search
#' #' @return List of httr2 responses
#' #' @noRd
#'
#' get_resps_datastore_search <- function(resource_id, records_format = "csv", ...) {
#'   resps <- get_req_datastore_search(resource_id, records_format = "csv", ...) |>
#'     httr2::req_perform_parallel()
#'   return(resps)
#' }


#' Get basic information about a resource
#'
#' @param resource_id Character string of resource ID
#' @return List containing number of rows ('nrow') and column names ('col_names')
#' @noRd
get_resource_base_info <- function(resource_id,filters) {
  query_vec <- c(total = "/result/total", fields = "/result/fields")
  base_names <- c(nrow= "total", col_names = "id")
  base_info <-
    get_resps_datastore_search(resource_id, records_format = "csv", limit = 0,filters=filters) |>
    extract_resps_info(query_vec) |>
    extract_fields(base_names) |>
    collapse::rm_stub("fields_") |>
    select_and_rename(base_names)
  return(as.list(base_info))
}


#' Process and optionally fix column names
#'
#' @param resource_base_info List containing basic resource information
#' @param fix_names Logical; whether to convert column names to snake_case
#' @return List of processed column names
#' @noRd
resource_col_names <- function(resource_base_info, fix_names) {
  col_names <- resource_base_info$col_names
  if (fix_names) {
    col_names <- Map(snakecase::to_snake_case, col_names)
  }
  return(col_names)
}

#' Calculate effective number of rows
#'
#' @param resource_base_info List containing basic resource information
#' @param max_row Maximum number of rows to return (NULL for all rows)
#' @return Vector of row counts
#' @noRd

resource_nrows <- function(resource_base_info, max_row) {
  n_rows <- resource_base_info$nrow
  max_row %|!|% {n_rows <- pmin(n_rows,max_row,na.rm = TRUE)}
  return(n_rows)

}



#' Generate sequence of offset values for pagination
#'
#' @param limit Number of records per page
#' @param total Total number of records
#' @return Numeric vector of offset values
#' @noRd

seq_offset <- function(limit, total) {
  if (total <= 32000) {
    return(0L)
  }
  seq.int(from = 0, to = total, by = limit)
}

#' Read CSV records into Arrow Table
#'
#' @param records Raw CSV data
#' @param col_names Vector of column names
#' @return data.frame containing the parsed records
#' @noRd
read_records <- function(records, col_names) {
  arrow::read_csv_arrow(
    file = records,
    col_names = col_names,
    na = c("", "NA", "NULL")
  )
}

#' Retrieve records from resources
#'
#' @param resource_id Character vector of resource IDs
#' @param limit Maximum records per request
#' @param fields Character vector of fields to retrieve
#' @param offset_list List of offset values for pagination
#' @return List of records for each resource
#' @noRd
get_resource_records <- function(resource_id, limit, fields, filters,offset_list) {
  query_vec <- c(
    records = "/result/records",
    id = "/result/resource_id"
  )
  resps_info <-
    get_resps_datastore_search(resource_id,
                               limit = limit,
                               fields = fields,
                               filters = filters,
                               row_offset = offset_list,
                               include_total = FALSE) |>
    extract_resps_info(query_vec) |>
    collapse::rowbind(return = "list") |>
    collapse::ftransformv("records",I)
  records <- collapse::rsplit(resps_info$records,resps_info$id,sort =FALSE)
  return(records)
}





#' Download Data from the data.gov.il Datastore
#'
#' The main data retrieval function. Downloads records from one or more
#' resources in the data.gov.il datastore, with automatic pagination,
#' optional field selection, filtering, and column name normalization.
#'
#' @param resource_id Resource identifier(s). Accepts a character vector of
#'   IDs, a [gidi_resource] object, a [list_gidi_resource], or data.gov.il
#'   URLs. Use a named vector to label the results
#'   (e.g., `c(crimes = "id1", traffic = "id2")`).
#' @param fields Character or numeric vector of fields to retrieve.
#'   `NULL` (default) returns all fields. When querying multiple resources,
#'   pass a list of vectors for per-resource field selection.
#' @param filters A named list of filter conditions applied server-side
#'   (e.g., `list(city = "Jerusalem")`). For multiple resources with
#'   different filters, pass a list of named lists.
#' @param add_name Controls result naming when multiple resources are queried.
#'   `TRUE` (default) fetches display names from the API; `FALSE` leaves
#'   results unnamed; a character vector assigns custom names.
#' @param limit Maximum records per API request (default/max: 32000).
#'   Pagination is handled automatically.
#' @param max_row Maximum total rows to return. `NULL` (default) returns all.
#' @param fix_names If `TRUE` (default), column names are converted to
#'   snake_case via [snakecase::to_snake_case()].
#'
#' @return A [data.table::data.table] for a single resource, or a named list
#'   of data.tables for multiple resources.
#'
#' @family data retrieval
#' @seealso [gidi_resources_by_pak()] to discover resource IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' # Single resource
#' dt <- gidi_datastore("abc123")
#'
#' # Select specific fields and filter
#' dt <- gidi_datastore("abc123",
#'   fields = c("name", "date"),
#'   filters = list(city = "Jerusalem")
#' )
#'
#' # Multiple resources with custom names
#' dts <- gidi_datastore(c(crimes = "abc123", traffic = "def456"))
#'
#' # Limit rows, keep original column names
#' dt <- gidi_datastore("abc123", max_row = 1000, fix_names = FALSE)
#' }
gidi_datastore <- function(resource_id, fields = NULL, filters = NULL, add_name = TRUE, limit = NULL,max_row = NULL,fix_names = TRUE) {

  limit <- limit %||% 32000
  max_row %|!|% {limit <- min(limit,max_row)}
  resource_id_len <- length(resource_id)
  resource_id <- check_resource_id(resource_id)

  if (rlang::is_named(resource_id)) {
    add_name <- names(resource_id)
  } else {
    if (is.character(add_name) &&
        length(add_name) != resource_id_len) {
      add_name <- TRUE
    }
  }
  resource_base_info <-  get_resource_base_info(resource_id, filters)
  col_names <-  resource_col_names(resource_base_info,fix_names)
  n_rows <- resource_nrows(resource_base_info,max_row)
  offset_list <- Map(seq_offset,limit =  limit,total = n_rows)
  offset_list <- class_list_numeric(offset_list)

  records <- get_resource_records(resource_id, limit, fields, filters, offset_list)
  records <- Map(read_records,records,col_names)
  records <- Map(collapse::qDT, records)


  if (resource_id_len == 1) {
    records <- records[[1]]
  } else if (is.character(add_name)) {
    names(records) <- add_name
  } else if (add_name) {
    names(records) <- get_resource_name(resource_id)
  }



  return(records)
}


# --------------------------------------

#' Check and validate resources ID
#'
#' @param resource_id resources ID to validate. Can be character string, gidi_resource object,
#'        or list of gidi_resource objects
#' @return Validated resources ID(s)
#' @noRd
check_resource_id <- S7::new_generic("check_resource_id", "resource_id")

#' Method for character input
#'
#' @param resource_id Character string of resources ID
#' @return If empty string, returns list of all org names, otherwise returns resource_id
#' @noRd
S7::method(check_resource_id, S7::class_character) <-
  function(resource_id) {
    if (all_data_gov_url(resource_id)) {
      url_to_id(resource_id, "resource")
    } else {
      resource_id
    }
  }

#' Method for gidi_resource object
#'
#' @param resource_id gidi_resource object
#' @return resources ID extracted from object
#' @noRd
S7::method(check_resource_id, gidi_resource)  <- function(resource_id) {
  x <-  S7::prop(resource_id, "resource_id")
  if (!rlang::is_empty(x)) {
    return(x)
  }else {
    return(S7::prop(resource_id, "pak_eng_name"))
  }
}
#' Method for list of gidi_resource objects
#'
#' @param resource_id List of gidi_resource objects
#' @return Vector of resources IDs extracted from objects
#' @noRd
S7::method(check_resource_id, list_gidi_resource)  <- function(resource_id) {
  mapply(check_resource_id,resource_id,USE.NAMES = TRUE)
}

