# ============================================================
# Tests for gidi_org S7 class
# ============================================================

test_that("gidi_org can be created with valid inputs", {
  org <- gidi_org(id = "abc123", eng_name = "test-org", heb_name = "ארגון")
  expect_true(S7::S7_inherits(org, gidi_org))
  expect_equal(org@id, "abc123")
  expect_equal(org@eng_name, "test-org")
  expect_equal(org@heb_name, "ארגון")
})

test_that("gidi_org_full_info inherits from gidi_org", {
  org <- gidi_org_full_info(
    id = "abc123",
    eng_name = "test-org",
    heb_name = "ארגון",
    packages = 5,
    resources = 20
  )
  expect_true(S7::S7_inherits(org, gidi_org))
  expect_true(S7::S7_inherits(org, gidi_org_full_info))
  expect_equal(org@packages, 5)
  expect_equal(org@resources, 20)
})

test_that("as_gidi_org creates gidi_org from named list", {
  org <- as_gidi_org(list(id = "123", eng_name = "test", heb_name = "בדיקה"))
  expect_true(S7::S7_inherits(org, gidi_org))
})

test_that("as_gidi_org creates gidi_org_full_info when extra fields present", {
  org <- as_gidi_org(list(
    id = "123", eng_name = "test", heb_name = "בדיקה",
    packages = 3, resources = 10
  ))
  expect_true(S7::S7_inherits(org, gidi_org_full_info))
})

test_that("as_gidi_org rejects unnamed input", {
  expect_error(as_gidi_org(list("a", "b", "c")))
})

test_that("gidi_org converts to data.frame", {
  org <- gidi_org(id = "123", eng_name = "test", heb_name = "בדיקה")
  df <- as.data.frame(org)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 1)
  expect_true("eng_name" %in% names(df))
})
