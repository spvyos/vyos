create_vyos_environment <- function() {
  vyos_env <- base::new.env(parent = emptyenv())
  return(vyos_env)
}

# vyos Package namespace ......................................................
# ..................vyos
# vyos package env
vyos_env <- create_vyos_environment()

set_vyos_test_value <- function(.value = TRUE) {
  vyos_env$test <- .value

  .value
}

get_vyos_test_value <- function() {
  return(vyos_env$test)
}

switch_vyos_test <- function() {
  vyos_env$test <- !vyos_env$test
}


clean_vyos_environment <- function() {
  rm(list = ls(envir = vyos_env), envir = vyos_env)
}
