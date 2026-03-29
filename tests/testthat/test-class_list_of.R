# ============================================================
# Tests for class_list_of and typed list classes
# ============================================================

test_that("class_list_numeric validates elements", {
  nums <- class_list_numeric(list(1, 2, 3))
  expect_true(S7::S7_inherits(nums, class_list_numeric))

  expect_error(class_list_numeric(list("a", "b")))
})

test_that("class_list_character validates elements", {
  chars <- class_list_character(list("a", "b", "c"))
  expect_true(S7::S7_inherits(chars, class_list_character))

  expect_error(class_list_character(list(1, 2)))
})

test_that("list_gidi_org validates all elements are gidi_org", {
  org1 <- gidi_org(id = "1", eng_name = "org1", heb_name = "ארגון1")
  org2 <- gidi_org(id = "2", eng_name = "org2", heb_name = "ארגון2")

  org_list <- list_gidi_org(list(`ארגון1` = org1, `ארגון2` = org2))
  expect_true(S7::S7_inherits(org_list, list_gidi_org))
  expect_length(org_list, 2)
})

test_that("list_gidi_org rejects non-org elements", {
  pak <- gidi_pak(
    pak_id = "1", pak_heb_name = "חבילה", pak_eng_name = "pkg",
    org_heb_name = "ארגון", org_eng_name = "org"
  )
  expect_error(list_gidi_org(list(pak)))
})

test_that("gidi_objs_list subsetting preserves class", {
  org1 <- gidi_org(id = "1", eng_name = "org1", heb_name = "ארגון1")
  org2 <- gidi_org(id = "2", eng_name = "org2", heb_name = "ארגון2")
  org_list <- list_gidi_org(list(`ארגון1` = org1, `ארגון2` = org2))

  subset <- org_list[1]
  expect_true(S7::S7_inherits(subset, list_gidi_org))
  expect_length(subset, 1)
})

test_that("gidi_objs_list as.data.frame works", {
  org1 <- gidi_org(id = "1", eng_name = "org1", heb_name = "ארגון1")
  org2 <- gidi_org(id = "2", eng_name = "org2", heb_name = "ארגון2")
  org_list <- list_gidi_org(list(`ארגון1` = org1, `ארגון2` = org2))

  df <- as.data.frame(org_list)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2)
})

test_that("can_be_list_of returns TRUE for valid lists", {
  expect_true(can_be_list_of(list(1, 2, 3), S7::class_numeric))
  expect_true(can_be_list_of(list("a", "b"), S7::class_character))
})

test_that("can_be_list_of returns FALSE for invalid lists", {
  expect_false(can_be_list_of(list("a", "b"), S7::class_numeric))
})

test_that("can_be_list_of errors on non-list input", {
  expect_error(can_be_list_of(42, S7::class_numeric))
})
