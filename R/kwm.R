# Loops over a vector of patterns, returning TRUE once the first match is made.
# If no matches are found, returns FALSE
first_match <- function(x, p, opts) {
  for (i in seq_along(p)) {
    res <- do.call(grepl, args = c(list(pattern = p[i], x = x), opts))
    if (res) {
      return(TRUE)
    }
  }
  return(FALSE)
}

allowed_grepl_opts <- names(formals(grepl))[-(1:2)]
print_grepl_opts <- glue::collapse(allowed_grepl_opts, sep = ", ")

#' Produce a model object based on regular expressions
#'
#' @param include Character. List of regular expression patterns, any one of which will return a positive match.
#' @param exclude Character. List of regular expression patterns, any one of which will return a negative match, overriding any other matches with `include`
#' @param varname Character. A character vector of length one with the column name to be tested.
#' @param grepl_opts Named list of arguments to pass to \link{grepl}.
#'
#' @export
#'
#' @return `kwm` returns a model object of class `kwm_model`
kwm <- function(include, exclude = character(), varname, grepl_opts = NULL) {
  assertthat::assert_that(is.character(include), msg = "include must be a character vector")
  assertthat::assert_that(is.character(exclude), msg = "exclude must be a character vector")
  assertthat::assert_that(assertthat::is.string(varname), msg = "varname must be a character vector of length one")
  assertthat::assert_that(all(names(grepl_opts) %in% allowed_grepl_opts), msg = glue::glue("Allowed options for grepl include {print_grepl_opts}"))

  l <- list(
    include = include,
    exclude = exclude,
    varname = varname,
    grepl_opts = grepl_opts
  )
  class(l) <- "kwm"
  l
}

#' @rdname kwm
#'
#' @param object A `kwm` model object
#' @param newdata A data.frame containing a text column
#' @param progress Logical. Display a progress bar?
#' @param return_names Logical. Add original text as names to the resulting
#'   logical vector?
#'
#' @return `predict.kwm` reutrns a logical vector.
#'
#' @export
predict.kwm <- function(object, newdata, progress = interactive(), return_names = FALSE) {

  newdata_name <- deparse(substitute(newdata, env = .GlobalEnv))

  assertthat::assert_that(
    assertthat::has_name(newdata, object$varname),
    msg = glue::glue("{newdata_name} does not have a column named '{object$varname}'."))

  assertthat::assert_that(
    is.character(newdata[[object$varname]]),
    msg = glue::glue("'{object$varname}' in {newdata_name} is not character."))

  progress_allowed <- progress & requireNamespace("progress", quietly = TRUE)

  x <- newdata[[object$varname]]

  if (progress_allowed) pb <- progress::progress_bar$new(
    format = " Predicting [:bar] :percent eta: :eta",
    total = length(x), clear = FALSE, width = 60)

  vapply(x, function(y) {
    if (progress_allowed) pb$tick()
    first_match(y, object$include, opts = object$grepl_opts) & !first_match(y, object$exclude, opts = object$grepl_opts)
  }, FUN.VALUE = logical(1),
  USE.NAMES = return_names)
}
