#' @include gidi_org.R gidi_pak.R gidi_resource2.R


#' Open data.gov.il item in browser
#'
#' Generic function to open the web page of a data.gov.il item
#' (package, organization, or resource) in the default web browser.
#'
#' @param x  `gidi_pak`, `gidi_org`, `gidi_resource`, or `list_gidi_org`
#' @export

gidi_see <- S7::new_generic("gidi_see", "x")

S7::method(gidi_see, gidi_pak) <-  function(x) {
  url <-  stringi::stri_join("https://data.gov.il/dataset/", x@pak_eng_name)
  utils::browseURL(url)
}

S7::method(gidi_see, gidi_org) <-  function(x) {
  url <-  stringi::stri_join("https://data.gov.il/organization/", x@eng_name)
  utils::browseURL(url)
}

S7::method(gidi_see, gidi_resource) <-  function(x) {
  url <-  stringi::stri_join("https://data.gov.il/dataset/",
                             x@pak_eng_name,
                             "/resource/",
                             x@resource_id)
  utils::browseURL(url)
}

S7::method(gidi_see, gidi_objs_list) <-  function(x) {
  if (!rlang::is_scalar_list(x)) {
    warning("Only the first list element will be displayed in browser")
  }
  gidi_see(x[[1]])
  }



