is_vyos_GETPREP <- function(x) {
  inherits(x, "vyos_GETPREP")
}

choose_fnc_for_source <- function(source = "evds", base = "series") {
  fnc_str <- "null"

  if (source == "evds" & base == "series") {
    fnc_str <- "evds_series_fnc"
  }
  if (source == "evds" & base == "table") {
    fnc_str <- "evds_table_fnc"
  }
  if (source == "fred") {
    fnc_str <- "fred_series_fnc"
  }
  fnc_str
}

vec_choose_fnc_for_source <- Vectorize(choose_fnc_for_source)

assign_data_funcs <- function(dots_params) {
  df <- dots_params$lines %>% dplyr::mutate(fnc_str = "null")
  df <- df %>% dplyr::mutate(fnc_str = vec_choose_fnc_for_source(source, base))
  dots_params$lines <- df
  dots_params
}


default_start_date <- function() {
  lubridate::ymd("2000/01/01")
}


default_end_date <- function() {
  lubridate::ymd("2100/01/01")
}

get_series_prepare <- function(index = null,
                               start_date = default_start_date(),
                               end_date = default_end_date(),
                               freq = null,
                               cache = FALSE,
                               na.remove = TRUE,
                               verbose = FALSE,
                               ...,
                               source = c("multi", "evds", "fred"),
                               base = c("multi", "series", "table")) {
  if (is.null(index)) {
    # index <-   template_test()
    msg <- "
    index should be given to request data from sources.
    see README file or type `?get_series` for documentation and examples.
    "
    stop(msg, call. = F)
  }

  if (verbose) {
    verbose_on()
  } else {
    verbose_off()
  }


  rlang::check_required(index)
  lines <- get_lines_as_df(index)
  lines$freq <- rep(to_string(freq), nrow(lines))
  call. <- deparse(match.call())
  dots_params <- create_params_list(c(as.list(environment()), list(...)))
  dots_params$status <- "ready_to_run"
  dots_params <- add_class(dots_params, "vyos_GETPREP")
  dots_params
}


#' Requests data from multiple data sources.
#' EDDS API and FRED API at this version.
#' @description
#' The get_series() function requests data from data sources.
#' when more than one index was given as a character vector or a string template
#' it requests all items individually from corresponding sources by figuring out
#' from the format of item given.
#' Combines a data frame when there is a common frequencies. Returns all data
#' both as a combined data frame and also individual data frames.
#' @param index character  vector or string
#' @param start_date limits the start date of data
#' @param end_date  limits the end date of data
#' @param freq frequency rarely needed
#' @param cache cache option if false new request will be made
#' @param na.remove it will safely remove NA values only if all columns are NA
#' @param verbose if TRUE it prints some information during the process. If FALSE
#'  silently does its job. Gives warning only if something goes wrong.
#' @param ... for future versions
#' @param debug debug option for development
#' @param source source such as evds or fred for internal use for at this version
#' @param base table or series on the source for internal use for at this version
#' @return vyos_GETPREP S3 object
#' @export
#' @examples
#' \dontrun{
#' o <- get_series(template_test())
#' excel(o)
#' object <- get_series("UNRATE", start_date = "2000/01/01", na.remove = TRUE)
#' excel(object)
#' }
get_series <- function(index = null,
                       start_date = default_start_date(),
                       end_date = default_end_date(),
                       freq = null,
                       cache = FALSE,
                       na.remove = TRUE,
                       verbose = FALSE,
                       ...,
                       source = c("multi", "evds", "fred"), # for internal use
                       base = c("multi", "series", "table"), # for internal use
                       debug = FALSE) {

  check_users_choice_if_cache(cache)

  obj <- get_series_prepare(
    index      = index,
    source     = source,
    base       = base,
    start_date = start_date,
    end_date   = end_date,
    freq.      = freq,
    cache      = cache,
    na.remove  = na.remove,
    verbose    = verbose
  )

  if (debug) {
    return(obj)
  }
  obj <- gets(obj)
  obj
}
