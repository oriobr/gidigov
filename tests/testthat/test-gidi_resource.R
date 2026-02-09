# ============================================================
# Tests for gidi_resource S7 class
# ============================================================

test_that("gidi_resource can be created with valid inputs", {
  res <- gidi_resource(
    resource_id = "res-001",
    pak_heb_name = "חבילה",
    pak_eng_name = "test-package",
    resource_heb_name = "משאב",
    size = 1024,
    last_modified = as.POSIXct("2024-01-01"),
    created = as.POSIXct("2023-06-15")
  )
  expect_true(S7::S7_inherits(res, gidi_resource))
  expect_equal(res@resource_id, "res-001")
  expect_equal(res@size, 1024)
})

test_that("gidi_resource @see is a function", {
  res <- gidi_resource(
    resource_id = "res-001",
    pak_heb_name = "חבילה",
    pak_eng_name = "test-package",
    resource_heb_name = "משאב",
    size = 512,
    last_modified = as.POSIXct("2024-01-01"),
    created = as.POSIXct("2023-01-01")
  )
  expect_type(res@see, "closure")
})

test_that("as_gidi_resource creates gidi_resource from named list", {
  res <- as_gidi_resource(list(
    resource_id = "res-002",
    pak_heb_name = "חבילה",
    pak_eng_name = "pkg",
    resource_heb_name = "משאב",
    size = 256,
    last_modified = as.POSIXct("2024-01-01"),
    created = as.POSIXct("2023-01-01")
  ))
  expect_true(S7::S7_inherits(res, gidi_resource))
})

test_that("gidi_resource converts to data.frame", {
  res <- gidi_resource(
    resource_id = "res-001",
    pak_heb_name = "חבילה",
    pak_eng_name = "pkg",
    resource_heb_name = "משאב",
    size = 100,
    last_modified = as.POSIXct("2024-01-01"),
    created = as.POSIXct("2023-01-01")
  )
  df <- as.data.frame(res)
  expect_s3_class(df, "data.frame")
  expect_true("resource_id" %in% names(df))
})
