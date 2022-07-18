# Clase 'loadings' (psych):

assert_is_loading_matrix <- function(
  x, severity = getOption("assertive.severity", "stop")
) {

  assertive.base::assert_engine(
    is_loading_matrix, x,
    .xname = get_name_in_parent(x),
    severity = severity
  )
}

is_loading_matrix <- function(x, .xname = get_name_in_parent(x)) {

  is2(x, "loadings", .xname)
}
