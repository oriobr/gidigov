
#' Create an Autocomplete Request for data.gov.il
#'
#' Builds an httr2 request targeting the CKAN v2 autocomplete endpoint for
#' either organizations or datasets.
#'
#' @param q Character. The search query string.
#' @param req_type Character. One of `"organization"` or `"dataset"`.
#' @return An [httr2::request] object (not yet performed).
#' @export
data_gov_autocomplete_req <- function(q,req_type = c("organization","dataset")) {
  req_type <- match.arg(req_type)
  data_gov_base_req |>
    httr2::req_url_path_append("api/2/util") |>
    httr2::req_url_path_append(req_type) |>
    httr2::req_url_path_append("autocomplete") |>
    httr2::req_url_query(q=q)
}

#' Create a resource show request
#'
#' @param resource_id Character string of resource ID
#' @return An httr2 request object
#' @noRd
req_resource_show_ <- function(resource_id) {
  data_gov_api_req("resource_show") |>
    httr2::req_url_query(id = resource_id)
}

#' Vectorized version of req_resource_show_
#' @noRd
req_resource_show <- Vectorize(req_resource_show_, SIMPLIFY = FALSE, USE.NAMES = FALSE)

#' @noRd
NULL

#' Get Resource Metadata
#'
#' Retrieves detailed metadata for one or more resources from the
#' data.gov.il `resource_show` endpoint. Requests are executed in parallel.
#'
#' @param resource_id Character vector of resource IDs.
#' @return A data.frame with one row per resource containing all metadata
#'   fields returned by the API (ID, name, format, size, dates, etc.).
#'
#' @family resource info
#' @export
get_resource_info <- function(resource_id) {
  resps <- req_resource_show(resource_id) |>
    httr2::req_perform_parallel()
  df_resource_info <-
    extract_resps_info(resps, "/result") |>
    collapse::rowbind(return = "data.frame",fill = TRUE)
  return(df_resource_info)
}

#' Get a Resource's Display Name
#'
#' Convenience wrapper around [get_resource_info()] that returns only the
#' resource name.
#'
#' @param resource_id Character string of a single resource ID.
#' @return Character string -- the resource's display name.
#'
#' @family resource info
#' @export
get_resource_name <- function(resource_id) {
  get_resource_info(resource_id)$name
}

#' Look Up Resource Metadata by Name
#'
#' Searches for a resource by its display name using the `package_search`
#' endpoint and returns the first matching resource's metadata.
#'
#' @param database_name Character vector of resource names to look up.
#' @return A data.table with one row per name containing resource metadata.
#'
#' @family resource info
#' @export
get_resource_info_by_name <- function(database_name) {
  resps <-
    lapply(database_name,
           \(database_name){
             data_gov_api_req("package_search") |>
               httr2::req_url_query(q = database_name)
           }) |>
    httr2::req_perform_parallel()

  resources <-
    extract_resps_info(resps,"/result/results/0/resources") |>
    data.table::rbindlist()
  resources <- resources[.(database_name),on = "name",mult = "first"]
  return(resources)
}

#' Get a Resource ID by Name
#'
#' Convenience wrapper around [get_resource_info_by_name()] that returns
#' only the resource ID.
#'
#' @param database_name Character string of the resource name.
#' @return Character string -- the resource's unique ID.
#'
#' @family resource info
#' @export
get_resource_id_by_name <- function(database_name) {
  get_resource_info_by_name(database_name)$resource_id
}

#' Get Package Information
#'
#' Retrieves metadata for all active (datastore-enabled) resources within
#' a given package from the `package_show` endpoint.
#'
#' @param package_id Character string of a package ID or English name.
#' @return A [data.table::data.table] of active resources in the package.
#'
#' @family resource info
#' @export
get_package_info <- function(package_id) {
  resp <-
    data_gov_api_req("package_show") |>
    httr2::req_url_query(id = package_id) |>
    httr2::req_perform()

  resources <- resp$body |>
    RcppSimdJson::fparse("/result/resources") |>
    collapse::qDT()

  return(collapse::sbt(resources,datastore_active == TRUE))
}



