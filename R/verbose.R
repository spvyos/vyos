verbose_off <- function() {
  options("VYOS_verbose" = FALSE)
}
verbose_on <- function() {
  options("VYOS_verbose" = TRUE)
}



check_verbose_option <- function() {
  .check <- getOption("VYOS_verbose")
  ..f2 <- function() {
    check_verbose_option()
    options(VYOS_verbose = TRUE)
    check_verbose_option()
    options(VYOS_verbose = FALSE)
    check_verbose_option()
  }
  if (is.null(.check)) {
    options(VYOS_verbose = FALSE)
    return(FALSE)
  }
  return(.check)
}

print_if_verbose <- function(msg) {
  .check <- check_verbose_option()
  if (!.check) {
    return(inv(NULL))
  }
  .blue(msg)
  return(inv(NULL))
}
