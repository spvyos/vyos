#' Turn Off Verbose Mode
#'
#' This function turns off verbose mode, suppressing additional informational
#' output. It is useful when you want to limit the amount of information
#' displayed during the execution of certain operations.
#'
#' @details
#' Verbose mode is often used to provide detailed information about the
#' progress of a function or operation. By calling \code{verbose_off}, you can
#' disable this additional output.
#'
#' The \code{options("VYOS_verbose" = FALSE)} line sets the verbose option to
#' \code{FALSE}, silencing additional messages.
#'
#' @return
#' The function has no return value.
#'
#' @export
#'
#' @examples
#' verbose_off()
#'
#' @seealso
#' \code{\link{verbose_on}}: Turn on verbose mode.
#'
verbose_off <- function() {
  options("VYOS_verbose" = FALSE)
  success_force("Verbose mode is now OFF. You may call `verbose_on()` anytime to enable it.")
}


#' Turn On Verbose Mode
#'
#' This function turns on verbose mode, enabling additional informational
#' output. It is useful when you want to receive detailed information about
#' the progress of certain operations.
#'
#' @details
#' Verbose mode is designed to provide detailed information during the execution
#' of a function or operation. By calling \code{verbose_on}, you can enable
#' this additional output.
#'
#' The \code{options("VYOS_verbose" = TRUE)} line sets the verbose option to
#' \code{TRUE}, allowing functions to produce more detailed messages.
#'
#' @return
#' The function has no explicit return value.
#'
#' @export
#'
#' @examples
#' verbose_on()
#'
#' @seealso
#' \code{\link{verbose_off}}: Turn off verbose mode.
#'
verbose_on <- function() {
  options("VYOS_verbose" = TRUE)
  success_force("Verbose mode is now ON.\n")
  success_force("You will receive additional information during function execution.\n")

  inv(NULL)
}




check_verbose_option <- function() {
  .check <- getOption("VYOS_verbose")

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
