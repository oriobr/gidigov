# ============================================================
# Tests for utility functions
# ============================================================

test_that("is_data_gov_url correctly identifies data.gov.il URLs", {
  expect_true(is_data_gov_url("https://data.gov.il/dataset/test"))
  expect_false(is_data_gov_url("https://example.com/data"))
  expect_false(is_data_gov_url("http://data.gov.il/test"))
})

test_that("all_data_gov_url checks all URLs", {
  expect_true(all_data_gov_url(c(
    "https://data.gov.il/dataset/a",
    "https://data.gov.il/dataset/b"
  )))
  expect_false(all_data_gov_url(c(
    "https://data.gov.il/dataset/a",
    "https://example.com/b"
  )))
})

test_that("data_gov_url_info extracts type and id", {
  info <- data_gov_url_info("https://data.gov.il/dataset/criminal-offenses/resource/abc123")
  expect_equal(info$type, "resource")
  expect_equal(info$id, "abc123")
})

test_that("url_to_id converts resource URL to id", {
  id <- url_to_id("https://data.gov.il/dataset/test/resource/abc123", "resource")
  expect_equal(id, "abc123")
})

test_that("url_to_id rejects mismatched type", {
  expect_error(
    url_to_id("https://data.gov.il/dataset/test/resource/abc123", "org")
  )
})

test_that("url_to_id rejects invalid type argument", {
  expect_error(url_to_id("https://data.gov.il/dataset/test", "invalid_type"))
})

test_that("%|!|% operator works correctly", {
  expect_equal(NULL %|!|% "fallback", NULL)
  expect_equal("value" %|!|% "fallback", "fallback")
})

test_that("ensure_list wraps atomic values", {
  result <- ensure_list("hello")
  expect_true(is.list(result))
  expect_length(result, 1)
})

test_that("ensure_list keeps nested lists as-is", {
  input <- list(list(1), list(2))
  result <- ensure_list(input)
  expect_equal(result, input)
})

test_that("is_atomic_list identifies atomic lists", {
  expect_true(is_atomic_list(list(1, "a", TRUE)))
  expect_false(is_atomic_list(list(list(1))))
  expect_false(is_atomic_list("not a list"))
})

test_that("is_atomic_named_list checks names", {
  expect_true(is_atomic_named_list(list(a = 1, b = 2)))
  expect_false(is_atomic_named_list(list(1, 2)))
})
