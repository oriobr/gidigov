#' @include gidi_org.R gidi_pak.R gidi_resource2.R

#' Create package show request
#'
#' @param package_id Character string of package ID
#' @return An httr2 request object
#' @noRd
req_package_show <- function(package_id) {
  data_gov_api_req("package_show") |>
    httr2::req_url_query(id = package_id)
}

#' Process resources data frame
#'
#' @param resources List or data frame of resources from API response
#' @param package_name Package name information extracted from API response
#' @param single_resource  Logical indicating if resources are not nested
#' @return Processed data frame with resources information, including:
#'         - Converted date columns
#'         - Added package information
#'         - Filtered active resources
#'         - Sorted by creation date and format
#'         - Removed duplicates
#' @noRd
process_resources <- function(resources, package_name, single_resource) {
  # Convert to data frame
  if (single_resource) {
    df_resources <- collapse::qDT(resources)
  } else {
    df_resources <- collapse::rowbind(resources,
                                      idcol = "package",
                                      return = "data.table",
                                      fill = TRUE)
  }
  # Convert date columns
  collapse::settransformv(df_resources, c("created", "last_modified"), as.POSIXct, tz = "Asia/Jerusalem")

  # Add package information
  if (!single_resource) {
    collapse::add_vars(df_resources) <- collapse::ss(package_name, df_resources$package)
  } else {
    df_resources <- cbind(df_resources, package_name)
  }

  # Filter and sort
  df_resources <- collapse::sbt(df_resources, datastore_active == TRUE)
  data.table::setorder(df_resources, -created, format)

  # Remove duplicates
  df_resources <- df_resources |>
    collapse::gby(name, package_title) |>
    collapse::ffirst()

  return(df_resources)
}



#' Check and validate organization ID
#'
#' @param pak_id Organization ID to validate. Can be character string, gidi_pak object,
#'        or list of gidi_pak objects
#' @return Validated organization ID(s)
#' @noRd
check_pak_id <- S7::new_generic("check_pak_id", "pak_id")

#' Method for character input
#'
#' @param pak_id Character string of organization ID
#' @return If empty string, returns list of all org names, otherwise returns pak_id
#' @noRd
S7::method(check_pak_id, S7::class_character) <-
  function(pak_id) {
    if (all_data_gov_url(pak_id)) {
      url_to_id(pak_id, "pak")
    } else {
      pak_id
    }
  }

#' Method for gidi_pak object
#'
#' @param pak_id `gidi_pak` object
#' @return Organization ID extracted from object
#' @noRd
S7::method(check_pak_id, gidi_pak)  <- function(pak_id) {
  x <-  S7::prop(pak_id, "pak_id")
  if (!rlang::is_empty(x)) {
    return(x)
  }else {
    return(S7::prop(pak_id, "pak_eng_name"))
  }
}
#' Method for list of gidi_pak objects
#'
#' @param pak_id List of gidi_pak objects
#' @return Vector of organization IDs extracted from objects
#' @noRd
S7::method(check_pak_id, list_gidi_pak)  <- function(pak_id) {
  sapply(pak_id, check_pak_id, USE.NAMES = FALSE)
}
S7::method(check_pak_id, S7::class_list)  <- function(pak_id) {
  sapply(pak_id, check_pak_id, USE.NAMES = FALSE)
}




#' Get resources by package
#'
#' Retrieves resource information for specified packages from the data.gov.il API.
#'
#' @param package_id Character vector of package IDs
#' @param as_data.frame Logical. If TRUE returns results as a data frame,
#'        if FALSE returns nested list
#' @return If as_data.frame is TRUE, returns a data frame with columns:
#'         resource_id, pak_heb_name, pak_eng_name, resource_heb_name,
#'         created, size, last_modified.
#'         If FALSE, returns a nested list of resources grouped by package.
#' @export
#' @examples
#' # Get resources for specific package
#' resources <- gidi_resources_by_package("package123")
#'
#' # Get resources as data frame
#' resources_df <- gidi_resources_by_package("package123", as_data.frame = TRUE)
gidi_resources_by_pak <- function(package_id, as_data.frame = FALSE) {
  package_id <- check_pak_id(package_id)
  # Make API requests
  resps <-
    Map(req_package_show, package_id) |>
    httr2::req_perform_parallel()

  # Define query paths
  query_package <- c(
    package_title = "/result/title",
    package_name = "/result/name",
    resources = "/result/resources",
    org_eng_name =  "/result/organization/name"
  )

  # Extract response information
  resps_info <- extract_resps_info(resps, query_package)
  package_name <- extract_fields(resps_info, c("package_title", "package_name","org_eng_name"))
  resources <- collapse::get_elem(resps_info, "resources")
  single_resource  <- is.data.frame(resources)

  # Process resources data frame
  df_resources <- process_resources(resources, package_name, single_resource)

  # Select and rename columns
  resource_var <- c(
    resource_id = "id",
    pak_heb_name = "package_title",
    pak_eng_name = "package_name",
    resource_heb_name = "name",
    "org_eng_name",
    "created",
    "size",
    "last_modified"
  )
  df_resources <- select_and_rename(df_resources, resource_var)

  if (as_data.frame) {
    return(df_resources)
  }

  # Create nested list structure
  by_fml = by_formula(names(df_resources), "resource_heb_name")
  if (!single_resource) {
    by_fml <- update(by_fml, ~ pak_heb_name + .)
  }

  df_resources <- gidi_nest_list(x = df_resources,
                                 by = by_fml,
                                 FUN_convert = as_gidi_resource,
                                 FUN_as_list = list_gidi_resource)
  return(df_resources)
}
