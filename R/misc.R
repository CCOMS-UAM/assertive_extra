## Funciones para clases específicas: ------------------------------------------

# Comprobación de clases:

#' Title
#'
#' @param x
#' @param class
#' @param severity
#'
#' @return
#' @export
#'
#' @examples
assert_is_of_class <- function(
  x, class, severity = getOption("assertive.severity", "stop")
) {

	assert_engine(
	  is2, x, class, .xname = get_name_in_parent(x), severity = severity
	)
}


assert_class_is_one_of <- function(
  x, classes, severity = getOption("assertive.severity", "stop")
) {

	assert_engine(
	  class_is_one_of, x, classes, .xname = get_name_in_parent(x),
	  severity = severity
	)
}

class_is_one_of <- function(x, classes, .xname = get_name_in_parent(x)) {

	if (length(classes) == 0L)
		stop(ERROR_PROVIDE_CLASS)

	ok <- any(class(x) %in% classes)

	if (!ok) {
		return(
			false(
				"class of %s is not one of '%s'; it is %s.",
				.xname, classes %>% paste0(collapse = ", "),
				assertive.base:::type_description(x)
			)
		)
	}
	TRUE
}


assert_are_of_same_class <- function(x, y,
                                     severity = getOption(
                                       "assertive.severity",
                                       "stop"
                                     )) {
  assert_engine(
    are_of_same_class, x, y,
    .xname = get_name_in_parent(x), .yname = get_name_in_parent(y),
    severity = severity
  )
}

are_of_same_class <- function(x, y,
                              .xname = get_name_in_parent(x),
                              .yname = get_name_in_parent(y)) {
  ok <- class(x) == class(y)

  if (!ok) {
    return(false("'%s' and '%s' are of different classes.", .xname, .yname))
  }
  TRUE
}


## Funciones para Shiny: -------------------------------------------------------

assert_is_a_reactive <- function(
  x, severity = getOption("assertive.severity", "stop")
) {

	assert_engine(
	  is_a_reactive, x, .xname = get_name_in_parent(x), severity = severity
	)
}

is_a_reactive <- function(x, .xname = get_name_in_parent(x)) {

	is2(x, "reactive", .xname)
}


## Otras funciones: ------------------------------------------------------------

assert_is_not_missing <- function(
  x, .xname = get_name_in_parent(x),
  severity = getOption("assertive.severity", "stop")
) {

	assert_engine(
	  is_not_missing, x, .xname = get_name_in_parent(x), severity = severity
	)
}

is_not_missing <- function(x, .xname = get_name_in_parent(x)) {

	if(missing(x)) return(false("argument %s is missing", .xname))

	return(TRUE)
}
