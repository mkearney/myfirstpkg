#' Find regular expression matches
#'
#' Return first or all regular expression matches
#'
#' @param x Input character vector
#' @param pat Regular expression pattern/search string
#' @param perl Logical indicating whether to use perl-style regex.
#'   If NULL (default) this checks for perl-style expressions.
#' @param ... Other arguments passed to \code{\link{regexpr}} or
#'   \code{\link{gregexpr}}
#' @examples
#'
#' ## find first match
#' re_match(c("AbC", "dEf"), "[A-Z]")
#'
#' ## find all matches
#' re_matches(c("AbC", "dEf"), "[A-Z]")
#'
#' @return A list containing character vector matches for each input
#' @export
re_match <- function(x, pat, perl = NULL, ...) {
  if (is.null(perl)) {
    perl <- is_perl(pat)
  }
  m <- regexpr(pat, x, perl = perl, ...)
  y <- vapply(m, function(.x) .x > 0, FUN.VALUE = logical(1))
  o <- character(length(x))
  o[y] <- regmatches(x, m)
  o
}

#' @inheritParams re_match
#' @rdname re_match
#' @export
re_matches <- function(x, pat, perl = NULL, ...) {
  if (is.null(perl)) {
    perl <- is_perl(pat)
  }
  m <- gregexpr(pat, x, perl = perl, ...)
  regmatches(x, m)
}

is_perl <- function(pat) {
  ## upper and lowercase replacements OR
  grepl("\\U|\\L|\\E", pat) ||

    ## hyphen at start/end of bracketed char class OR
    grepl("(\\[\\-.*\\])|(\\[.*\\-\\])", pat) ||

    ## use of grouping/capturing (?.*)
    grepl("\\(\\?.*\\)", pat)
}
