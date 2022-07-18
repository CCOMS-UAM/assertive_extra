# Clase 'Datafile' (no implementada):

assert_ID_is_not_duplicate <- function(
  x, severity = getOption("assertive.severity", "stop")
) {

  assertive.base::assert_engine(
    ID_is_not_duplicate, x, .xname = get_name_in_parent(x), severity = severity
  )
}

ID_is_not_duplicate <- function(x, .xname = get_name_in_parent(x)) {

  assert_if_of_class(x, "character") # TODO: esto es para comprobar un vector de nombres de variables con IDs

  n_ids <- x %>% equals("ID") %>% sum

  if (n_ids %>% is_greater_than(1))
    return(false(MESSAGE_ID_DUPLICATE, .xname))

  TRUE
}
