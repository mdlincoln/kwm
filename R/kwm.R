# Loops over a vector of patterns, returning TRUE once the first match is made.
# If no matches are found, returns FALSE
first_match <- function(x, p) {
  any(stringr::str_detect(x, stringr::regex(p, ignore_case = TRUE)))
}

#' Produce a model object based on regular expressions
#'
#' @param include Character. List of regular expression patterns, any one of which will return a positive match.
#' @param exclude Character. List of regular expression patterns, any one of which will return a negative match, overriding any other matches with `include`
#' @param varname Character. A character vector of length one with the column name to be tested.
#'
#' @export
#'
#' @return A moel object of class `kwm_model`
kwm <- function(include, exclude = character(), varname) {
  assertthat::assert_that(is.character(include), msg = "include must be a character vector")
  assertthat::assert_that(is.character(exclude), msg = "exclude must be a character vector")
  assertthat::assert_that(assertthat::is.string(varname), msg = "varname must be a character vector of length one")

  l <- list(
    include = include,
    exclude = exclude,
    varname = varname
  )
  class(l) <- "kwm"
  l
}

#' @export
predict.kwm <- function(object, newdata, progress = interactive()) {

  newdata_name <- deparse(substitute(newdata, env = .GlobalEnv))

  assertthat::assert_that(
    assertthat::has_name(newdata, object$varname),
    msg = stringr::str_glue("{newdata_name} does not have a column named '{object$varname}'."))

  assertthat::assert_that(
    is.character(newdata[[object$varname]]),
    msg = stringr::str_glue("'{object$varname}' in {newdata_name} is not character."))

  progress_allowed <- progress & requireNamespace("progress", quietly = TRUE)

  x <- newdata[[object$varname]]

  if (progress_allowed) pb <- progress::progress_bar$new(
    format = " Predicting [:bar] :percent eta: :eta",
    total = length(x), clear = FALSE, width = 60)

  vapply(x, function(y) {
    if (progress_allowed) pb$tick()
    first_match(y, object$include) & !first_match(y, object$exclude)
  }, FUN.VALUE = logical(1),
  USE.NAMES = FALSE)
}
