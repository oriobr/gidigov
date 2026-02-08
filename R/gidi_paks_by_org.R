#' @include gidi_org.R gidi_pak.R gidi_resource2.R



#' Create organization show request
#'
#' @param organization_id Character string of organization ID
#' @return An httr2 request object
#' @noRd
req_org_show <- function(org_id) {
  data_gov_api_req("organization_show") |>
    httr2::req_url_query(
      id = org_id,
      include_datasets = TRUE,
      include_dataset_count = FALSE,
      include_extras = FALSE,
      include_users = FALSE,
      include_groups = FALSE,
      include_tags = FALSE,
      include_followers = FALSE
    )
}

#' Check and validate organization ID
#'
#' @param org_id Organization ID to validate. Can be character string, gidi_org object,
#'        or list of gidi_org objects
#' @return Validated organization ID(s)
#' @noRd
check_org_id <- S7::new_generic("check_org_id","org_id")

#' Method for character input
#'
#' @param org_id Character string of organization ID
#' @return If empty string, returns list of all org names, otherwise returns org_id
#' @noRd
S7::method(check_org_id, S7::class_character) <-
  function(org_id) {
    if (org_id == "") {
      gidi_organization_list(names_only = TRUE)
    } else {
      org_id
    }
  }

#' Method for gidi_org object
#'
#' @param org_id gidi_org object
#' @return Organization ID extracted from object
#' @noRd
S7::method(check_org_id, gidi_org)  <- function(org_id) {S7::prop(org_id,"id")}

#' Method for list of gidi_org objects
#'
#' @param org_id List of gidi_org objects
#' @return Vector of organization IDs extracted from objects
#' @noRd
S7::method(check_org_id, list_gidi_org)  <- function(org_id) {sapply(org_id,S7::prop,name = "id" ,USE.NAMES = FALSE)}

#' Get paks by organization
#'
#' Retrieves package information for specified organizations from the data.gov.il API.
#'
#' @param org_id Character vector of organization IDs. If empty string, retrieves for all organizations
#' @param as_data.frame Logical. If TRUE returns results as a data frame, if FALSE returns nested list
#' @return If as_data.frame is TRUE, returns a data frame with columns:
#'         pak_id, pak_heb_name, pak_eng_name, org_heb_name, org_eng_name.
#'         If FALSE, returns a nested list of paks grouped by organization.
#' @export
#' @examples
#' # Get paks for specific org
#' paks <- gidi_paks_by_org("org123")
#'
#' # Get paks for all orgs
#' all_paks <- gidi_paks_by_org()
#'
#' # Get results as data frame
#' paks_df <- gidi_paks_by_org("org123", as_data.frame = TRUE)
gidi_paks_by_org <- function(org_id = "", as_data.frame = FALSE) {
  org_id <- check_org_id(org_id)
  resps <-
    Map(req_org_show,org_id) |>
    httr2::req_perform_parallel()
  pak_in_org <-
    extract_resps_info(resps, "/result/packages", max_simplify_lvl = "list") |>
    extract_fields(names = c("title", "id", "name"))
  pak_var <-
    c(pak_id = "id",
      pak_heb_name = "title",
      pak_eng_name = "name",
      org_heb_name = "organization_title",
      org_eng_name = "organization_name")
  pak_in_org <- select_and_rename(pak_in_org,pak_var)
  if(as_data.frame){return(pak_in_org)}
  by_frml = by_formula(names(pak_in_org),"pak_heb_name")
  if (length(org_id) > 1) {
    by_frml <- update(by_frml,.~org_heb_name+.)
  }
  pak_in_org <- gidi_nest_list(pak_in_org, by = by_frml, as_gidi_pak, list_gidi_pak)
  return(pak_in_org)
}
