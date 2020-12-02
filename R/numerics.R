assert_is_a_whole_number <- function(
  x, severity = getOption("assertive.severity", "stop")
) {
  assert_engine(is_a_whole_number, x, .xname = get_name_in_parent(x),
                severity = severity)
}

is_a_whole_number <- function(
  x, tol = 100 * .Machine$double.eps, .xname = get_name_in_parent(x)
) {

  if (!(ok <- is_scalar(x, .xname = .xname)))
    return(ok)
  if (!(ok <- is_whole_number(x, tol = tol, .xname = .xname)))
    return(ok)
  TRUE
}


assert_is_less_than_or_equal_to <- function(
  x, y, severity = getOption("assertive.severity", "stop")
) {
  assert_engine(is_a_scalar_less_than_or_equal_to, x, y,
                .xname = get_name_in_parent(x), severity = severity)
}

is_a_scalar_less_than_or_equal_to <- function(
  x, y, .xname = get_name_in_parent(x), .yname = get_name_in_parent(y)
) {
  assert_is_a_number(y)

  if (!(ok <- is_scalar(x, .xname = .xname)))
    return(ok)
  if (!(ok <- is_less_than_or_equal_to(x, y, .xname, .yname)))
    return(ok)
  TRUE
}


assert_is_greater_than_or_equal_to <- function(
  x, y, severity = getOption("assertive.severity", "stop")
) {
  assert_engine(is_a_scalar_greater_than_or_equal_to, x, y,
                .xname = get_name_in_parent(x), severity = severity)
}

is_a_scalar_greater_than_or_equal_to <- function(
  x, y, .xname = get_name_in_parent(x), .yname = get_name_in_parent(y)
) {
  if (!(ok <- is_scalar(x, .xname = .xname)))
    return(ok)
  if (!(ok <- is_greater_than_or_equal_to(x, y, .xname, .yname)))
    return(ok)
  TRUE
}


assert_is_a_non_negative_integer <- function(
  x, severity = getOption("assertive.severity", "stop")
) {
  assert_engine(is_a_non_negative_integer, x,
                .xname = get_name_in_parent(x), severity = severity)
}

is_a_non_negative_integer <- function(x, .xname = get_name_in_parent(x)) {

  if (!(ok <- is_a_scalar_greater_than_or_equal_to(x, 0, .xname)))
    return(ok)
  if (!(ok <- is_whole_number(x, .xname = .xname)))
    return(ok)
  TRUE
}


assert_is_a_non_positive_integer <- function(
  x, severity = getOption("assertive.severity", "stop")
) {
  assert_engine(is_a_non_positive_integer, x,
                .xname = get_name_in_parent(x), severity = severity)
}

is_a_non_positive_integer <- function(x, .xname = get_name_in_parent(x)) {

  if (!(ok <- is_a_scalar_less_than_or_equal_to(x, 0, .xname)))
    return(ok)
  if (!(ok <- is_whole_number(x, .xname = .xname)))
    return(ok)
  TRUE
}


assert_is_a_natural_number <- function(
  x, severity = getOption("assertive.severity", "stop")
) {
  assert_engine(is_a_natural_number, x,
                .xname = get_name_in_parent(x), severity = severity)
}

#' Title
#'
#' @param x
#' @param .xname
#'
#' @return
#' @export
#' @importFrom assertive.numbers is_greater_than
#'
#' @examples
is_a_natural_number <- function(x, .xname = get_name_in_parent(x)) {

  if (!(ok <- is_a_whole_number(x, .xname = .xname)))
    return(ok)
  if (!(ok <- assertive.numbers::is_greater_than(x, 0, .xname)))
    return(ok)
  TRUE
}


assert_is_a_negative_integer <- function(
  x, severity = getOption("assertive.severity", "stop")
) {
  assert_engine(is_a_negative_integer, x,
                .xname = get_name_in_parent(x), severity = severity)
}

is_a_negative_integer <- function(x, .xname = get_name_in_parent(x)) {

  if (!(ok <- is_a_whole_number(x, .xname = .xname)))
    return(ok)
  if (!(ok <- is_less_than(x, 0, .xname)))
    return(ok)
  TRUE
}


assert_is_a_zero <- function(
  x, severity = getOption("assertive.severity", "stop")
) {
  assert_engine(is_zero, x,
                .xname = get_name_in_parent(x), severity = severity)
}

is_zero <- function(
  x, tol = 100 * .Machine$double.eps, .xname = get_name_in_parent(x)
) {
  if (!(ok <- is_equal_to(x, 0, tol = tol, .xname = .xname)))
    return(ok)
  TRUE
}

assert_all_are_zero <- function(
  x, tol = 100 * .Machine$double.eps, na_ignore = FALSE,
  severity = getOption("assertive.severity", "stop")
) {
  .xname <- get_name_in_parent(x)

  msg <- gettextf("%s are not all equal to 0 (tol = %g).", .xname, tol)
  assert_engine(
    is_zero, x, tol = tol, .xname = .xname,
    msg = msg, na_ignore = na_ignore, severity = severity
  )
}

assert_any_are_zero <- function(
  x, tol = 100 * .Machine$double.eps, na_ignore = FALSE,
  severity = getOption("assertive.severity", "stop")
) {
  .xname <- get_name_in_parent(x)

  msg <-  gettextf("%s are never equal to 0 (tol = %g).", .xname, tol)
  assert_engine(
    is_zero, x, tol = tol, .xname = .xname,
    msg = msg, na_ignore = na_ignore, severity = severity
  )
}


assert_is_in_range <- function(
  x, lower, upper, lower_is_strict = FALSE, upper_is_strict = FALSE,
  severity = getOption("assertive.severity", "stop")
) {
  assert_engine(is_a_scalar_in_range, x, lower = lower, upper = upper,
                lower_is_strict = lower_is_strict,
                upper_is_strict = upper_is_strict,
                .xname = get_name_in_parent(x), severity = severity)
}

is_a_scalar_in_range <- function(
  x, lower = -Inf, upper = Inf,
  lower_is_strict = FALSE, upper_is_strict = FALSE,
  .xname = get_name_in_parent(x)
) {
  if (!(ok <- is_scalar(x, .xname = .xname)))
    return(ok)
  if (!(ok <- is_in_range(
    x, lower = lower, upper = upper,
    lower_is_strict = lower_is_strict,
    upper_is_strict = upper_is_strict, .xname = .xname
  )))
    return(ok)
  TRUE
}


assert_none_is_in_range <- function(x, lower, upper,
                                    lower_is_strict = FALSE,
                                    upper_is_strict = FALSE,
                                    severity = getOption(
                                      "assertive.severity",
                                      "stop"
                                    )) {
  assert_engine(
    is_not_in_range, x,
    lower = lower, upper = upper,
    lower_is_strict = lower_is_strict, upper_is_strict = upper_is_strict,
    .xname = get_name_in_parent(x), severity = severity
  )
}

is_not_in_range <- function(
  x, lower = -Inf, upper = Inf,
  lower_is_strict = FALSE, upper_is_strict = FALSE,
  .xname = get_name_in_parent(x)
) {
  ok <- !is_in_range(x, lower = lower, upper = upper,
                     lower_is_strict = lower_is_strict,
                     upper_is_strict = upper_is_strict,
                     .xname = .xname)
  if (!ok) {
    names(ok) <- x
    set_cause(ok, ifelse(too_low, "too low", "too high"))
  }
  TRUE
}
