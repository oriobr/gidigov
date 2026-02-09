# ============================================================
# Tests for gidi_pak S7 class
# ============================================================

test_that("gidi_pak can be created with valid inputs", {
  pak <- gidi_pak(
    pak_id = "pkg-001",
    pak_heb_name = "חבילה",
    pak_eng_name = "test-package",
    org_heb_name = "ארגון",
    org_eng_name = "test-org"
  )
  expect_true(S7::S7_inherits(pak, gidi_pak))
  expect_equal(pak@pak_id, "pkg-001")
  expect_equal(pak@pak_eng_name, "test-package")
})

test_that("as_gidi_pak creates gidi_pak from named list", {
  pak <- as_gidi_pak(list(
    pak_id = "pkg-001",
    pak_heb_name = "חבילה",
    pak_eng_name = "test-package",
    org_heb_name = "ארגון",
    org_eng_name = "test-org"
  ))
  expect_true(S7::S7_inherits(pak, gidi_pak))
})

test_that("as_gidi_pak rejects non-list input", {
  expect_error(as_gidi_pak("not a list"))
})

test_that("as_gidi_pak rejects list with NULL values", {
  expect_error(as_gidi_pak(list(
    pak_id = NULL,
    pak_heb_name = "חבילה",
    pak_eng_name = "test",
    org_heb_name = "ארגון",
    org_eng_name = "org"
  )))
})

test_that("gidi_pak converts to data.frame", {
  pak <- gidi_pak(
    pak_id = "001", pak_heb_name = "חבילה", pak_eng_name = "pkg",
    org_heb_name = "ארגון", org_eng_name = "org"
  )
  df <- as.data.frame(pak)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 1)
  expect_true("pak_id" %in% names(df))
})
