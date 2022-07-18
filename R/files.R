## Funciones para el sistema de ficheros: --------------------------------------

assert_is_an_existing_file <- function(
  x, severity = getOption("assertive.severity", "stop")
) {

  assertive.base::assert_engine(
    is_an_existing_file, x,
    .xname = get_name_in_parent(x),
    severity = severity
  )
}

is_an_existing_file <- function(x, .xname = get_name_in_parent(x)) {

  if (!(ok <- is_existing_file(x, .xname)))
    return(ok)
  if (!(ok <- is_scalar(x, .xname = .xname)))
    return(ok)
  TRUE
}

assert_is_a_readable_file <- function(
  x, warn_about_windows = TRUE,
  severity = getOption("assertive.severity", "stop")
) {

  assertive.base::assert_engine(
    is_a_readable_file, x,
    warn_about_windows = warn_about_windows, .xname = get_name_in_parent(x),
    severity = severity
  )
}

is_a_readable_file <- function(
  x, warn_about_windows = warn_about_windows, .xname = get_name_in_parent(x)
) {

  if (!(ok <- is_scalar(x, .xname = .xname)))
    return(ok)
  if (
    !(
      ok <- is_readable_file(
        x, warn_about_windows = warn_about_windows, .xname = .xname
      )
    )
  )
    return(ok)
  TRUE
}

assert_is_a_directory <- function(
  x,
  severity = getOption("assertive.severity", "stop")
) {

  assertive.base::assert_engine(
    is_a_directory, x, .xname = get_name_in_parent(x), severity = severity
  )
}

is_a_directory <- function(x, .xname = get_name_in_parent(x)) {

  if (!(ok <- is_dir(x, .xname)))
    return(ok)
  if (!(ok <- is_scalar(x, .xname = .xname)))
    return(ok)
  TRUE
}
