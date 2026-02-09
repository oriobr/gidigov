# ============================================================
# Tests for S7 utility functions
# ============================================================

test_that("class_checker works with character class names", {
  expect_true(class_checker(data.frame(), "data.frame"))
  expect_false(class_checker(1, "data.frame"))
})

test_that("class_checker works with S7 classes", {
  org <- gidi_org(id = "1", eng_name = "test", heb_name = "בדיקה")
  expect_true(class_checker(org, gidi_org))
  expect_false(class_checker("not an org", gidi_org))
})

test_that("class_checker works with S7 union", {
  org <- gidi_org(id = "1", eng_name = "test", heb_name = "בדיקה")
  pak <- gidi_pak(
    pak_id = "1", pak_heb_name = "חבילה", pak_eng_name = "pkg",
    org_heb_name = "ארגון", org_eng_name = "org"
  )
  union_class <- gidi_org | gidi_pak
  expect_true(class_checker(org, union_class))
  expect_true(class_checker(pak, union_class))
})

test_that("is_S7_object detects S7 objects", {
  org <- gidi_org(id = "1", eng_name = "test", heb_name = "בדיקה")
  expect_true(is_S7_object(org))
  expect_false(is_S7_object("plain string"))
  expect_false(is_S7_object(42))
})

test_that("check_S7_class accepts valid S7 classes", {
  expect_no_error(check_S7_class(gidi_org))
})

test_that("check_S7_class rejects non-S7 classes", {
  expect_error(check_S7_class("not a class"))
  expect_error(check_S7_class(42))
})
