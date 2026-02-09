#' @include props.R
#' @include req.R

# ============================================================
# S7 class hierarchy for building datastore API requests
# ============================================================
# Three-level chain:
#   req_datastore_args      -- holds and validates query parameters
#     -> req_datastore_builder  -- splits params into dots/MoreArgs for .mapply()
#       -> resps_datastore_search -- adds @perform() to execute requests in parallel
#
# The builder pattern separates *what* to query from *how* to execute,
# and the computed properties (@dots, @MoreArgs, @build_requests, @perform)
# mean the actual HTTP calls are deferred until explicitly triggered.

req_datastore_args <- S7::new_class(
  name = "req_datastore_args",
  properties = list(
    resource_id = S7::new_property(S7::class_character),
    records_format =
      prop_enum(c("objects", "lists", "csv", "tsv"),
                default = "csv" ,
                allow_null = TRUE),
    limit =
      prop_number_whole(
        allow_null = TRUE,
        default = NULL,
        setter = function(self, value){
          self@limit <-  ( value %|!|% as.integer(value) )
          self
        }),
    fields =  S7::new_property(NULL | S7::class_character | class_list_character,default = NULL),
    filters = S7::new_property(NULL |S7::class_list,default = NULL),
    include_total = prop_bool(default = NULL, allow_null = TRUE),
    row_offset = S7::new_property(NULL | S7::class_numeric | class_list_numeric ,default = NULL)
  ),
  validator = function(self) {
    if (!list_length_match(self@fields, self@resource_id)) {
      "@fields is a list, but is not the same length as '@resource_id'"
    } else if (!list_length_match(self@row_offset, self@resource_id)) {
      "@row_offset is a list, but is not the same length as '@resource_id'"
    }
  }
)


# req_datastore_builder splits parameters for .mapply():
#  - @dots: per-resource arguments (vary across resources) -- vectorised
#  - @MoreArgs: shared arguments (same for every resource) -- recycled
# List-type params (e.g. per-resource field lists) go in dots;
# scalar/atomic params go in MoreArgs.
req_datastore_builder <- S7::new_class(
  name = "req_datastore_builder",
  parent = req_datastore_args,
  properties = list(
    dots = S7::new_property(S7::class_list,
      getter = function(self) {
        dots <- list(resource_id = self@resource_id)
        for (p in c("fields", "row_offset")) {
          if (is.list(S7::prop(self, p))) {
            dots[[p]] <- S7::prop(self, p)
          }
        }
        if (!is.null(self@filters)&& !self@filters@atomic ) {
          dots[["filters"]] <- self@filters
        }

        return(dots)
      }
    ),
    MoreArgs = S7::new_property(S7::class_list,
      getter = function(self) {
        MoreArgs <- list(
          limit = self@limit %|!|% as.integer(self@limit),
          records_format = self@records_format,
          include_total = self@include_total
        )

        for (p in c("fields", "row_offset")) {
          if (is.atomic(S7::prop(self, p))) {
            MoreArgs[[p]] <- S7::prop(self, p)
          }
        }
        if (!is.null(self@filters)&& self@filters@atomic ) {
          MoreArgs[["filters"]] <- self@filters
        }
        return(MoreArgs)
      }
    ),
    build_requests = S7::new_property(S7::class_function,
      getter = function(self) {
        function() {
          .mapply(
            req_datastore_search2,
            dots = self@dots,
            MoreArgs = self@MoreArgs
          ) |>
            collapse::vec()
        }
      }
    )
  )
)


# Final layer: adds @perform() -- a computed getter that returns a
# closure which calls httr2::req_perform_parallel() on the built requests.
resps_datastore_search <-
  S7::new_class(
    "resps_datastore_search",
    parent = req_datastore_builder,
    properties =
      list(perform = S7::new_property(S7::class_function,
        getter = function(self) {
          function() {
            httr2::req_perform_parallel(self@build_requests())
          }
        }
      ))
  )


# ================================
# Low-level request builder function (unchanged)
# ================================


req_datastore_search <-
  function(resource_id,
           records_format = NULL,
           limit = NULL,
           fields = NULL,
           include_total = NULL,
           row_offset = NULL) {
    req <-
      data_gov_api_req("datastore_search") |>
      httr2::req_url_query(
        resource_id = resource_id,
        records_format = records_format,
        limit = limit,
        fields = fields,
        include_total = include_total,
        .multi = "comma"
      )
    if (is.null(row_offset)) {
      return(list(req))
    } else {
      reqs <-
        .mapply(
          httr2::req_url_query,
          dots = list("offset" = collapse::vec(row_offset)),
          MoreArgs = list(.req = req)
        )
      return(reqs)
    }
  }


req_datastore_search2 <- function(resource_id,
                                  fields = NULL,
                                  filters = NULL,
                                  records_format = NULL,
                                  limit = NULL,
                                  include_total = NULL,
                                  row_offset = NULL) {

  body_json <-
  list(
    "filters" = filters,
    "resource_id" = resource_id,
    "records_format" = records_format,
    "limit" = limit,
    "fields" = fields,
    "include_total" = include_total
  )
  body_json <-
    collapse::get_elem(body_json,is.null,recursive = FALSE,invert = TRUE)

  req <- data_gov_api_req("datastore_search") |>
    httr2::req_body_json(
      body_json,
      null = "list"
    )

  if (is.null(row_offset)) {
    return(list(req))
  } else {
    reqs <-
      .mapply(
        httr2::req_body_json_modify,
        dots = list("offset" = collapse::vec(row_offset)),
        MoreArgs = list(req = req)
      )
    return(reqs)
  }
}


get_resps_datastore_search <- function(resource_id,...) {

  resps_datastore_search(
    resource_id = resource_id,
    ...
  )@perform()

}

