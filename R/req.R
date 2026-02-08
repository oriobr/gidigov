
#' Base request configuration for data.gov.il
#' @noRd
data_gov_base_req <- httr2::request("https://data.gov.il")

#
#' Create a data.gov.il API request
#'
#' Constructs an API request URL for the data.gov.il API endpoints
#'
#' @param req_type Character string specifying the API endpoint (e.g., "resource_show", "package_search")
#' @param ... Additional query parameters passed to req_url_query
#' @return An httr2 request object
#' @noRd
data_gov_api_req <- function(req_type = NULL,...) {
  data_gov_base_req |>
    httr2::req_url_path_append("api/3/action") |>
    httr2::req_url_path_append(req_type)
}

