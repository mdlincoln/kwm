
d <- data.frame(month = month.name, stringsAsFactors = FALSE)

context("Test kwm inputs")
test_that("Invalid inputs are rejected", {
  expect_error(kwm(include = integer(1)), regexp = "include")
  expect_error(kwm(exclude = integer(1)), regexp = "exclude")
  expect_error(kwm(include = "a", varname = c("a", "b")), regexp = "varname")
  expect_error(kwm(include = "a", varname = integer(1)), regexp = "varname")
})

context("Test predict.kwm inputs")
test_that("Invalid prediction outputs are rejected", {
  bad_kwm_model <- kwm(include = c("a", "y"), exclude = "r", varname = "foo")
  bad_newdata <- data.frame(foo = 1:10)
  expect_error(predict(bad_kwm_model, d), regexp = "d does not have a column named 'foo")
  expect_error(predict(bad_kwm_model, bad_newdata), regexp = "'foo' in bad_newdata is not character")
})

context("Test predict.kwm outputs")
test_that("Matches inclusions correctly", {
  kwm_model <- kwm(include = c("a", "y"), exclude = "r", varname = "month")
  caseless_kwm_model <- kwm(include = c("a", "y"), exclude = "r", varname = "month", search_opts = list(ignore_case = TRUE))
  kwm_predictions <- predict(kwm_model, newdata = d)
  caseless_kwm_predictions <- predict(caseless_kwm_model, newdata = d)
  named_kwm_predictions <- predict(kwm_model, newdata = d, return_names = TRUE)

  expect_equivalent(kwm_predictions, c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equivalent(caseless_kwm_predictions, c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_named(kwm_predictions, expected = NULL)
  expect_named(named_kwm_predictions)
})
