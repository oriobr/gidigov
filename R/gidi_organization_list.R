#' @include gidi_org.R gidi_pak.R gidi_resource2.R


#' Retrieve List of Organizations from Israeli Government Data Portal
#'
#' @description
#' Fetches organization information from the Israeli government's data portal API.
#' Can return either a full list of organization details or just organization names.
#'
#' @param names_only Logical. If TRUE, returns only organization names. If FALSE (default),
#'   returns complete organization information.
#' @param as_data.frame Logical. If TRUE and names_only is FALSE, returns results as a data frame.
#'   If FALSE (default), returns a list_gidi_org object.
#'
#' @return
#' Depending on the parameters:
#' - If names_only = TRUE: A character vector of organization names
#' - If names_only = FALSE and as_data.frame = TRUE: A data frame containing organization details
#' - If names_only = FALSE and as_data.frame = FALSE: A list_gidi_org object containing
#'   organization information
#'
#' The full organization information includes:
#' - id: Organization's unique identifier
#' - heb_name: Organization name in Hebrew
#' - eng_name: Organization name in English
#' - packages: Number of data packages published by the organization
#' - resources: Total number of resources across all packages
#'
#' @details
#' The function makes an API request to the Israeli government data portal,
#' processes the response, and standardizes the field names. It handles the
#' conversion of raw API data into more usable R objects.
#'
#' The API request is customized to minimize unnecessary data transfer by excluding
#' extras, users, groups, and tags when not needed.
#'
#' @examples
#' # Get only organization names
#' org_names <- gidi_organization_list(names_only = TRUE)
#'
#' # Get full organization information as a list_gidi_org
#' org_list <- gidi_organization_list()
#'
#' # Get full organization information as a data frame
#' org_df <- gidi_organization_list(as_data.frame = TRUE)
#'
#' @seealso
#' \code{\link{gidi_org}} for the organization class definition
#' \code{\link{list_gidi_org}} for the organization list class definition
#'
#' @note
#' The function requires an active internet connection and access to the
#' Israeli government data portal API. Network issues or API changes may
#' affect the function's behavior.
#'
#' @importFrom httr2 req_url_query req_perform
#' @importFrom rrapply rrapply
#' @importFrom collapse rm_stub frename rsplit
#'
#' @export


gidi_organization_list <- function(names_only = FALSE,
                                   as_data.frame = FALSE) {
  resp <-
    data_gov_api_req("organization_list") |>
    httr2::req_url_query(
      all_fields = !names_only,
      include_extras = FALSE,
      include_users = FALSE,
      include_groups = FALSE,
      include_tags = FALSE
    ) |>
    httr2::req_perform()

  org_list <- extract_resps_info(resp, "/result", max_simplify_lvl = "vector")

  if (names_only) {
    org_list <- unlist(org_list)
  } else {
    vars <-
      c(
        heb_name = "title",
        eng_name = "name",
        id = "id",
        packages = "package_count",
        resources = "resources_count"
      )
    org_list <-  extract_fields(org_list, vars)
    org_list <- collapse::rm_stub(org_list, "__extras_")
    org_list <- select_and_rename(org_list, vars)
    if (!as_data.frame) {
      org_list <-   gidi_nest_list(org_list,
                                   ~ heb_name,
                                   as_gidi_org,
                                   list_gidi_org ,
                                   keep.by = TRUE)
    }
  }
  return(org_list)
}
