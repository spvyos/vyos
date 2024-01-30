# ............................................... check_last_requested_from_source
check_last_requested_from_source <- function(kaynak = "evds") {
  last_time <- get_safe("last_request_times",
    default = list(
      evds = get_now(),
      fred = get_now()
    )
  )
  value <- ifelse(kaynak %in% names(last_time),
    last_time[[kaynak]],
    get_yesterday()
  )
  value
}
# ............................................... save_time_request_kaynak
save_time_request_kaynak <- function(kaynak = "evds") {
  last_time <- check_last_requested_from_source(kaynak)
  if (is.null(last_time)) {
    last_time <- list(
      evds = Sys.time(),
      fred = Sys.time()
    )
  } else {
    last_time[[kaynak]] <- Sys.time()
  }
  last_time
}
# ...................................................................... get_now
get_now <- function(num = 0) {
  time <- Sys.time()
  hrs <- lubridate::hours(num)
  mod_time <- time - hrs
  mod_time
}
# ...................................................................... get_yesterday
get_yesterday <- function() {
  get_now(24)
}
# ...................................................................... time_is_ok
time_is_ok <- function(last_requested_time = get_yesterday(), seconds = 1) {
  now_ <- get_now()
  fark <- now_ - last_requested_time # days
  diff_seconds <- (fark * 24 * 60) # *60/60
  diff_seconds > seconds
}
# ...................................................................... should_I_wait_for_request
should_I_wait_for_request <- function(source_name = "evds", seconds = 1, .verbose = FALSE) {
  last_request_time <- check_last_requested_from_source(source_name)
  last_request_time <- as.POSIXct(last_request_time, origin = "1970-01-01")
  if (time_is_ok(last_request_time, seconds)) {
    save_time_request_kaynak(source_name)
    return(inv(T))
  }
  msg <- "pausing before a new request."
  if (.verbose) {
    .blue("->[{source_name}]: {msg}")
  }
  # debug_message( "->[{source_name}]: {msg}")
  Sys.sleep(seconds)
  save_time_request_kaynak(source_name)
  return(inv(T))
}
# ...................................................................... test_should_I_wait_for_request
test_should_I_wait_for_request <- function() {
  should_I_wait_for_request("evds", 3) == T
  should_I_wait_for_request("evds", 3) == T
  should_I_wait_for_request("evds", 4) == F
  should_I_wait_for_request("evds", 2) == T
  should_I_wait_for_request("fred", 5) == F
  assert(
    should_I_wait_for_request("fred", 5) == T
  )
  .green("done")
}
