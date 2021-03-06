# Loops over a vector of patterns, returning TRUE once the first match is made.
# If no matches are found, returns FALSE
first_match <- function(x, p, search_fun, search_opts) {
  patternfun <- do.call(search_fun, args = c(list(pattern = p), search_opts))
  any(stringr::str_detect(string = x, pattern = patternfun))
}

allowed_grepl_opts <- names(formals(grepl))[-(1:2)]
print_grepl_opts <- paste0(allowed_grepl_opts, collapse = ", ")

#' Produce a model object based on regular expressions.
#'
#' @param include Character. List of regular expression patterns, any one of which will return a positive match.
#' @param exclude Character. List of regular expression patterns, any one of which will return a negative match, overriding any other matches with `include`
#' @param varname Character. A character vector of length one with the column name to be tested.
#' @param search_fun One of the [stringr::modifiers] functions from [stringr::stringr]. Defaults to [stringr::regex].
#' @param search_opts List of arguments to pass to `search_fun`.
#'
#' @export
#'
#' @return `kwm` returns a model object of class `kwm`
kwm <- function(include = character(), exclude = character(), varname, search_fun = stringr::regex, search_opts = NULL) {
  assertthat::assert_that(is.character(include), msg = "include must be a character vector")
  assertthat::assert_that(is.character(exclude), msg = "exclude must be a character vector")
  assertthat::assert_that(assertthat::is.string(varname), msg = "varname must be a character vector of length one")

  l <- list(
    include = include,
    exclude = exclude,
    varname = varname,
    search_fun = search_fun,
    search_opts = search_opts
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
#'   logical vector? Defaults to false.
#' @param parallel Logical. Compute predictions in parallel? Requires [doParallel].
#' @param ... Other arguments passed from functions. Not used currently.
#'
#' @return `predict.kwm` reutrns a logical vector.
#'
#' @export
predict.kwm <- function(object, newdata, progress = interactive(), return_names = FALSE, parallel = TRUE, ...) {

  newdata_name <- deparse(substitute(newdata))

  assertthat::assert_that(
    assertthat::has_name(newdata, object$varname),
    msg = stringr::str_glue("{newdata_name} does not have a column named '{object$varname}'."))

  assertthat::assert_that(
    is.character(newdata[[object$varname]]),
    msg = stringr::str_glue("'{object$varname}' in {newdata_name} is not character."))

  progress_allowed <- progress & requireNamespace("progress", quietly = TRUE)

  x <- newdata[[object$varname]]

  if (progress_allowed) {
    pb <- progress::progress_bar$new(
      format = " Predicting [:bar] :percent eta: :eta",
      total = length(x), clear = FALSE, width = 60)
  }

  if (parallel) {
    if (requireNamespace("doParallel", quietly = TRUE) && requireNamespace("foreach", quietly = TRUE)) {
      return(parallel_handler(x, object, progress_allowed, pb, return_names))
    } else {
      message("Parallel processing requires 'doParallel' and 'foreach'. Defaulting to single-threaded execution.")
    }
  }
  single_handler(x, object, progress_allowed, pb, return_names)
}

single_handler <- function(x, object, progress_allowed, pb = NULL, return_names) {
  vapply(x, function(y) {
    if (progress_allowed) pb$tick()
    predict_handler(y, object)
  }, FUN.VALUE = logical(1),
  USE.NAMES = return_names)
}

#' @import foreach
parallel_handler <- function(x, object, progress_allowed, pb = NULL, return_names) {
  doParallel::registerDoParallel()

  # Bind i so that R CMD CHECK does not throw a warning about an undefined
  # global variable when it is used in foreach
  i <- NULL

  res <- foreach(i = seq_along(x), .inorder = TRUE, .combine = c, .multicombine = TRUE) %dopar% {
    predict_handler(y = x[i], object)
  }
  if (return_names)
    names(res) <- x
  res
}

predict_handler <- function(y, object) {
  first_match(y, object$include, object$search_fun, object$search_opts) & !first_match(y, object$exclude, object$search_fun, object$search_opts)
}
