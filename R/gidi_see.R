#' @include gidi_org.R gidi_pak.R gidi_resource2.R


#' Open a data.gov.il Item in the Browser
#'
#' Opens the web page of a data.gov.il entity (organization, package, or
#' resource) in the default browser. Works with any gidi object or a
#' [gidi_objs_list] (opens the first element).
#'
#' @param x A [gidi_pak], [gidi_org], [gidi_resource], or [gidi_objs_list]
#'   object.
#' @return Invisibly returns `NULL`. Called for its side effect (opening
#'   a URL).
#'
#' @examples
#' \dontrun{
#' org <- gidi_organization_list()[[1]]
#' gidi_see(org)
#'
#' # Resources also have a shortcut property:
#' resource@see()
#' }
#'
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



