#' @include gidi_org.R gidi_pak.R gidi_resource2.R

#' Check if search returned valid results
#'
#' @param x List of search results
#' @return The input list if valid results exist
#' @throws Error if no results found
#' @noRd
check_search_result <- function(x) {
  if (rlang::is_scalar_list(x) && is.null(x[[1]])) {
    stop("No search results found")
  } else {
    x
  }
}

#' Generic search function factory for data.gov.il API
#'
#' @param autocomplete_type Type of autocomplete endpoint ("package_autocomplete" or "organization_autocomplete")
#' @param fields Named vector mapping API field names to desired output names
#' @param rhs Right-hand side of formula for grouping results
#' @param FUN_convert Function to convert individual results
#' @param FUN_as_list Function to convert group of results to list
#' @return A function that performs the search with parameters:
#'   - q: Search query string
#'   - as_data.frame: Boolean, if TRUE returns results as data frame instead of nested list
#'   @noRd
gidi_search_generic <- function(autocomplete_type, fields, rhs,
                                FUN_convert, FUN_as_list) {
  force(autocomplete_type)
  force(fields)
  force(rhs)
  force(FUN_convert)
  force(FUN_as_list)

  function(q, as_data.frame = FALSE) {
    # Make API request
    resp <- data_gov_api_req(autocomplete_type) |>
      httr2::req_url_query(q = q) |>
      httr2::req_perform()

    # Process response
    df_search <- resp |>
      extract_resps_info("/result", max_simplify_lvl = "list",
                         empty_array = NULL) |>
      check_search_result() |>
      extract_fields(fields) |>
      select_and_rename(fields)

    by_frml <- by_formula(names(df_search), rhs)

    if(as_data.frame) {
      return(df_search)
    }

    # Convert to nested list structure
    list_search <- gidi_nest_list(df_search, by = by_frml,
                                  FUN_convert, FUN_as_list)
    return(list_search)
  }
}

#' Search for packages in data.gov.il
#'
#' @param q Search query string
#' @param as_data.frame Boolean, if TRUE returns results as data frame
#' @return List or data frame of matching packages
gidi_search_pak <- gidi_search_generic(
  autocomplete_type = "package_autocomplete",
  fields = c(pak_eng_name = "name", pak_heb_name = "title"),
  rhs = ~pak_heb_name,
  FUN_convert = as_gidi_pak,
  FUN_as_list = list_gidi_pak
)

#' Search for organizations in data.gov.il
#'
#' @param q Search query string
#' @param as_data.frame Boolean, if TRUE returns results as data frame
#' @return List or data frame of matching organizations
gidi_search_org <- gidi_search_generic(
  autocomplete_type = "organization_autocomplete",
  fields = c(heb_name = "title", eng_name = "name", id = "id"),
  rhs = ~heb_name,
  FUN_convert = as_gidi_org,
  FUN_as_list = list_gidi_org
)

