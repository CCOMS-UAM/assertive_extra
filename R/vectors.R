
assert_length_is_equal_to_or_less_than <- function(
  x, len,
  severity = getOption("assertive.severity", "stop")
) {
  assertive.base::assert_engine(
    length_is_equal_to_or_less_than, x, len = len,
    .xname = assertive.base::get_name_in_parent(x),
    severity = severity
  )
}

length_is_equal_to_or_less_than <- function(
  x, len, .xname = assertive.base::get_name_in_parent(x)
) {

  len <- use_first(len)
  assertive.properties:::check_n(len)
  length_x <- length(x)
  if (length_x > len) {
    return(false(MESSAGE_LENGTH_NOT_EQUAL_TO_OR_LESS, .xname, length_x, len))
  }
  TRUE
}


assert_length_is_equal_to_or_more_than <- function(
  x, len,
  severity = getOption("assertive.severity", "stop")
) {
  assertive.base::assert_engine(
    length_is_equal_to_or_more_than, x, len = len,
    .xname = assertive.base::get_name_in_parent(x),
    severity = severity
  )
}

length_is_equal_to_or_more_than <- function(
  x, len, .xname = assertive.base::get_name_in_parent(x)
) {

  len <- use_first(len)
  assertive.properties:::check_n(len)
  length_x <- length(x)
  if (length_x > len) {
    return(false(MESSAGE_LENGTH_NOT_EQUAL_TO_OR_MORE, .xname, length_x, len))
  }
  TRUE
}


assert_are_different_length <- function (
  x, y,
  severity = getOption("assertive.severity", "stop")
) {
  assertive.base::assert_engine(
    are_different_length, x, y = y,
    .xname = assertive.base::get_name_in_parent(x),
    .yname = assertive.base::get_name_in_parent(y),
    severity = severity
  )
}

are_different_length <- function(
  x, y,
  .xname = assertive.base::get_name_in_parent(x),
  .yname = assertive.base::get_name_in_parent(y)
) {
  result <- are_same_length(x, y)

  if(result)
    return(false("%s and %s have the same length.", .xname, .yname))
  else return(TRUE)
}


assert_is_an_index <- function(
  x, severity = getOption("assertive.severity", "stop")
) {

  assertive.base::assert_engine(
    is_an_index, x,
    .xname = assertive.base::get_name_in_parent(x),
    severity = severity
  )
}

is_an_index <- function(x, .xname = assertive.base::get_name_in_parent(x)) {

  if(is_logical(x, .xname = .xname))	 return(TRUE)
  if(is_character(x, .xname = .xname)) return(TRUE)

  if(is_integer(x, .xname = .xname)) {

    if(!(ok <- is_positive(x, .xname = .xname)) %>% any) return(ok)
    else																								 return(TRUE)
  }
  if(is_numeric(x, .xname = .xname)) {

    if(!(ok <- is_positive(x, .xname = .xname)) %>% any)		 return(ok)
    if(!(ok <- is_whole_number(x, .xname = .xname)) %>% any) return(ok)

    return(TRUE)
  }

  set_cause(x, "class is not one of logical, character, integer, or numeric")
}


assert_is_exactly <- function(
  x, y, severity = getOption("assertive.severity", "stop")
) {

  assertive.base::assert_engine(
    is_exactly, x, y,
    .xname = assertive.base::get_name_in_parent(x),
    .yname = assertive.base::get_name_in_parent(y),
    severity = severity
  )
}

is_exactly <- function(
  x, y,
  .xname = assertive.base::get_name_in_parent(x),
  .yname = assertive.base::get_name_in_parent(y)
) {

  if(identical(x, y)) return(TRUE)
  if(all.equal(x, y)) return(TRUE)

  return(false(.xname, "is different from", .yname))
}


assert_have_same_names <- function(
  x, y, severity = getOption("assertive.severity", "stop")
) {

  assertive.base::assert_engine(
    have_same_names, x, y,
    .xname = assertive.base::get_name_in_parent(x),
    .yname = assertive.base::get_name_in_parent(y),
    severity = severity
  )
}

have_same_names <- function(
  x, y,
  .xname = assertive.base::get_name_in_parent(x),
  .yname = assertive.base::get_name_in_parent(y)
) {

  if(x %>% names %>% equals(y %>% names)) return(ok)

  return(
    false("The names of %s are different from the names of %s", .xname, .yname)
  )
}


assert_all_are_unique <- function(
  x, .xname = assertive.base::get_name_in_parent(x),
  severity = getOption("assertive.severity", "stop")
) {

  assertive.base::assert_engine(
    all_are_unique, x,
    .xname = assertive.base::get_name_in_parent(x),
    severity = severity
  )
}

all_are_unique <- function(x, .xname = assertive.base::get_name_in_parent(x)) {

  if (any(is.na(x))) {

    return(false("%s has 'NA' values", .xname))
  }


  if (length(x) != length(unique(x))) {

    return(false("%s has non unique values", .xname))
  }

  TRUE
}


assert_is_not_a_na <- function(x, coerce_to_logical = FALSE,
                               .xname = assertive.base::get_name_in_parent(x),
                               severity = getOption(
                                 "assertive.severity", "stop"
                               )) {

  assertive.base::assert_engine(
    is_not_a_na, x,
    coerce_to_logical = coerce_to_logical,
    .xname = assertive.base::get_name_in_parent(x),
    severity = severity
  )
}

is_not_a_na <- function(x, coerce_to_logical = FALSE,
                        .xname = assertive.base::get_name_in_parent(x)) {

  if (!assertive.properties::is_scalar(x, .xname = .xname)) return(TRUE)

  if (!(ok <- is_not_na(x, coerce_to_logical = coerce_to_logical,
                        .xname = .xname)))
    return(ok)

  TRUE
}


all_are_na <- function(x, .xname = assertive.base::get_name_in_parent(x)) {

  if (any(is_not_na(x))) {

    return(false("%s has non missing values", .xname))
  }

  TRUE
}
