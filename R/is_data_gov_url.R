#' Check if URL is from data.gov.il
#'
#' @param url Character vector of URLs to check
#' @return Logical vector indicating if each URL starts with "https://data.gov.il/"
#' @examples
#' is_data_gov_url("https://data.gov.il/dataset/123")
#'  @noRd
is_data_gov_url <- function(url) {
  stringi::stri_startswith_fixed(url, "https://data.gov.il/")
}

#' Check if all URLs are from data.gov.il
#'
#' @param url Character vector of URLs to check
#' @return Logical scalar indicating if all URLs are from data.gov.il
#' @examples
#'  @noRd
all_data_gov_url <- function(url) {
  all(is_data_gov_url(url))
}

#' Extract type and ID information from data.gov.il URLs
#'
#' @param url Character vector of data.gov.il URLs
#' @return Data frame with columns 'type' and 'id'
#' @noRd
data_gov_url_info <- function(url) {
  stringi::stri_match_all_regex(url, "^.*/(?<type>[^/]+)/(?<id>[^/]+)$") |>
    collapse::unlist2d(idcols = FALSE) |>
    collapse::gv(c("type", "id"))
}

#' Valid data.gov.il URL types
#' @noRd
.VALID_URL_TYPES <- c(resource = "resource" ,
                      org =  "organization",
                      pak  = "dataset")

#' Convert URL to ID based on type
#'
#' @param url Character vector of data.gov.il URLs
#' @param type Character scalar of expected URL type
#'        (one of: "resource", "organization", "dataset")
#' @return Character vector of IDs if URLs match type, otherwise original URLs
#'  @noRd
url_to_id <- function(url, type) {
  # Input validation
  if (!type %in% names(.VALID_URL_TYPES)) {
    stop("type must be one of: ", paste(names(.VALID_URL_TYPES), collapse = ", "))
  }

  # Process URLs
  df_url_info <- data_gov_url_info(url)
  if (!collapse::allv(df_url_info$type, .VALID_URL_TYPES[[type]])) {
    stop("Some URLs do not match the expected type:", type)
  }
  return(df_url_info$id)
}

#------------------------
