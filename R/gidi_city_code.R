#' Create an API request to CBS localities endpoint
#'
#' @param url_query List of query parameters
#' @return An httr2 request object
#' @keywords internal
city_code_req <- function(url_query) {
  httr2::request("https://api.cbs.gov.il/Dictionary/geo/localities") |>
    httr2::req_url_query(!!!url_query, .multi = "comma")
}

#' Extract and format locality data from API response
#'
#' @param x Raw API response data
#' @return A data frame with city codes and names
#' @keywords internal
extract_df_city_code <- function(x) {
  city_code_col_names <- c(
    city_code = "id",
    city_name_eng = "name_eng",
    city_name_heb = "name_heb"
  )

  df_city_code <-
    extract_fields(x, city_code_col_names) |>
    collapse::rm_stub("ID_") |>
    select_and_rename(city_code_col_names)

  return(df_city_code)
}

#' Update page number in query parameters
#'
#' @param page_number Page number to set
#' @param base_params Original query parameters
#' @return Updated query parameters list
#' @keywords internal
update_page_number <- function(page_number, base_params) {
  modifyList(base_params, list("page" = page_number))
}

#' Vectorized version of update_page_number
#' @keywords internal
update_page_numbers <- Vectorize(
  update_page_number,
  vectorize.args = "page_number",
  SIMPLIFY = FALSE
)

#' Generate query parameters for additional pages
#'
#' @param resp_info Response info containing pagination data
#' @param base_query Original query parameters
#' @return List of query parameters for remaining pages
#' @keywords internal
modify_queries_pages <- function(resp_info, base_query) {
  remaining_pages  <- 2:resp_info$last_page
  update_page_numbers(remaining_pages , base_query)
}

#' Query city codes from the Israeli CBS (Central Bureau of Statistics)
#'
#' @description
#' Retrieves city information from the CBS API, including codes and names
#' in Hebrew and English. Supports searching by ID or text query with different
#' matching options.
#'
#' @param city_id Optional numeric city ID to look up
#' @param q Optional search string for city names
#' @param match_type Character string specifying the matching type:
#'   "CONTAINS" (default), "BEGINS_WITH", or "EQUALS"
#' @return A data.table containing:
#'   \item{city_code}{Numeric city identifier}
#'   \item{city_name_eng}{English city name}
#'   \item{city_name_heb}{Hebrew city name}
#' @export
#'
#' @examples
#' # Search for cities containing "Tel"
#' cities <- gidi_city_code(q = "Tel", match_type = "CONTAINS")
#'
#' # Look up specific city by ID
#' city <- gidi_city_code(city_id = 5000)
#'
#' # Find cities beginning with "Bet"
#' bet_cities <- gidi_city_code(q = "Bet", match_type = "BEGINS_WITH")
gidi_city_code <- function(
    city_id = NULL,
    q = NULL,
    match_type = c("CONTAINS", "BEGINS_WITH", "EQUALS")
) {
  match_type <- match.arg(match_type)

  url_query <- list(
    expand = "false",
    fields = c("name_eng", "name_heb", "id"),
    format = "json",
    download = FALSE,
    page = "1",
    page_size = "250",
    string_match_type = match_type,
    q = q,
    id = city_id
  )

  # Initial request
  resp <-
    city_code_req(url_query) |>
    httr2::req_perform()

  # Extract response data
  json_query <- c(
    last_page = "/dictionary/paging/last_page",
    data = "/dictionary/data/localities/items"
  )

  resp_info <-
    extract_resps_info(resp, json_query, max_simplify_lvl = "list") |>
    unlist(recursive = FALSE)

  df_city_code <- extract_df_city_code(resp_info$data)

  # Handle pagination if needed
  if (resp_info$last_page > 1) {
    url_queries <- httr2::resp_url_queries(resp)

    additional_resps <-
      modify_queries_pages(resp_info, url_queries) |>
      lapply(city_code_req) |>
      httr2::req_perform_parallel()

    additional_info <-
      additional_resps |>
      extract_resps_info(json_query["data"], max_simplify_lvl = "list") |>
      extract_df_city_code()

    df_city_code <- rbind(df_city_code, additional_info)
  }

  # Convert data types and return as data.table
  df_city_code <- type.convert(df_city_code, as.is = TRUE)
  df_city_code <- collapse::qDT(df_city_code)

  return(df_city_code)
}
