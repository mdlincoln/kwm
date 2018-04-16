
d <- data.frame(month = month.name, stringsAsFactors = FALSE)

context("Test kwm inputs")
test_that("Invalid inputs are rejected", {
  expect_error(kwm(include = integer(1)))
  expect_error(kwm(exclude = integer(1)))
  expect_error(kwm(varname = c("a", "b")))
  expect_error(kwm(varname = integer(1)))
})

context("Test predict.kwm inputs")
test_that("Invalid prediction outputs are rejected", {
  bad_kwm_model <- kwm(include = c("a", "y"), exclude = "r", varname = "foo")
  bad_newdata <- data.frame(foo = 1:10)
  expect_error(predict(bad_kwm_model, d))
  expect_error(predict(bad_kwm_model, bad_newdata))
})

context("Test predict.kwm outputs")
test_that("Matches inclusions correctly", {
  kwm_model <- kwm(include = c("a", "y"), exclude = "r", varname = "month")
  caseless_kwm_model <- kwm(include = c("a", "y"), exclude = "r", varname = "month", grepl_opts = list(ignore.case = TRUE))
  kwm_predictions <- predict(kwm_model, newdata = d)
  caseless_kwm_predictions <- predict(caseless_kwm_model, newdata = d)
  named_kwm_predictions <- predict(kwm_model, newdata = d, return_names = TRUE)

  expect_equivalent(kwm_predictions, c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equivalent(caseless_kwm_predictions, c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_named(kwm_predictions, expected = NULL)
  expect_named(named_kwm_predictions)
})
