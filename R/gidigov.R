
#' Create autocomplete request for data.gov.il
#'
#' @param req_type Character string specifying type ("organization" or "dataset")
#' @param q Search query string
#' @return An httr2 request object
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

#' Create an offset request
#'
#' @param url Base URL
#' @param offset Offset value for pagination
#' @return An httr2 request object
#' @noRd

#' Get resource information from data.gov.il
#'
#' Retrieves detailed information about one or more resources
#'
#' @param resource_id Character vector of resource IDs
#' @return A data.table containing resource information
#' @export
get_resource_info <- function(resource_id) {
  resps <- req_resource_show(resource_id) |>
    httr2::req_perform_parallel()
  df_resource_info <-
    extract_resps_info(resps, "/result") |>
    collapse::rowbind(return = "data.frame",fill = TRUE)
  return(df_resource_info)
}

#' Get resource name by ID
#'
#' @param resource_id Character string of resource ID
#' @return Character string of resource name
#' @export
get_resource_name <- function(resource_id) {
  get_resource_info(resource_id)$name
}

#' Get resource information by name
#'
#' @param database_name Character string of database name
#' @return data.table of resource information
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

#' Get resource ID by name
#'
#' @param database_name Character string of database name
#' @return Character string of resource ID
#' @export
get_resource_id_by_name <- function(database_name) {
  get_resource_info_by_name(database_name)$resource_id
}

#' Get package information
#'
#' @param package_id Character string of package ID
#' @return data.table of active resources in the package
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




# -----


