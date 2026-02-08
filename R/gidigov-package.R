#' gidigov: Access Israel's Government Open Data Portal
#'
#' @description
#' An R interface to the Israeli government open data portal
#' (\url{https://data.gov.il}) and the Central Bureau of Statistics (CBS)
#' API. The package provides:
#'
#' - **Browsing**: List organizations, packages, and resources on the portal
#'   with [gidi_organization_list()], [gidi_paks_by_org()], and
#'   [gidi_resources_by_pak()].
#' - **Searching**: Find packages and organizations by keyword with
#'   [gidi_search_pak()] and [gidi_search_org()].
#' - **Data retrieval**: Download datastore records as [data.table::data.table]
#'   objects with [gidi_datastore()], with automatic pagination, field
#'   selection, and server-side filtering.
#' - **CBS integration**: Query city/locality codes with [gidi_city_code()].
#' - **Type-safe objects**: S7 classes ([gidi_org], [gidi_pak],
#'   [gidi_resource]) with validation, pretty-printing, and
#'   [as.data.frame()] support.
#'
#' @keywords internal
"_PACKAGE"


## usethis namespace: start
#' @import httr2
#' @import rlang
#' @import S7
#' @import collapse
#' @importFrom  RcppSimdJson fparse
## usethis namespace: end
NULL

