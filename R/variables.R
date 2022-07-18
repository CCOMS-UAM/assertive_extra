# Clase 'Variable' (no implementada);

assert_measurement_level_is <- function(
  x, level, severity = getOption("assertive.severity", "stop")
) {

  assertive.base::assert_engine(
    measurement_level_is, x, level,
    .xname = assertive.base::get_name_in_parent(x),
    severity = severity
  )
}

measurement_level_is <- function(x, level,
                                 .xname = assertive.base::get_name_in_parent(x)
                                 ) {

  assert_is_subset(level, NIVEL.MEDIDA)

  if(x %>% get.measurement.level %>% equals(level)) return(TRUE)
  else return(false(MESSAGE_MEASUREMENT_LEVEL_IS_NOT, .xname, level))
}


assert_measurement_level_is_one_of <- function(
  x, levels, severity = getOption("assertive.severity", "stop")
) {

  assertive.base::assert_engine(
    measurement_level_is_one_of, x, levels,
    .xname = assertive.base::get_name_in_parent(x),
    severity = severity
  )
}

measurement_level_is_one_of <- function(
  x, levels, .xname = assertive.base::get_name_in_parent(x)
) {

  assert_is_subset(levels, MEASUREMENT_LEVEL)

  if(get.measurement.level(x9) %in% levels) return(TRUE)

  else return(false(
    "The measurement level of %s is not one of %s",
    .xname, levels %>% collapse(sep = ", ", last = ", or ")
  ))
}
