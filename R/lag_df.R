
#' lag_df
#' Creates extra columns given as a list of column names and sequences.
#' useful to have multiple selection of lags of some columns while some columns to
#' be present only one or two lags or none.
#' @param df  data.frame or tibble
#' @param laglist a list of column names as index and lag sequence as value
#'
#' @return tibble
#' @export
#'
#' @examples
#'
#' lag_df( data.frame(list( a = 1: 5 ) , list(a = 1: 2 )) )
#'
lag_df<- function( df , laglist ){
    .Call(`_vyos_lag_df2_c`, df, laglist)
}

as_tibblex <- function(df){
    .Call(`_vyos_as_tibblex`, df)
}
#' lag_df2
#' Creates extra columns given as a list of column names and sequences.
#' useful to have multiple selection of lags of some columns while some columns to
#' be present only one or two lags or none.
#' @param df  data.frame or tibble
#' @param laglist a list of column names as index and lag sequence as value
#'
#' @return data.frame
#' @export
#'
#' @examples
#' lag_df2( data.frame(list( a = 1: 5 ) , list(a = 1: 2 )) )
#'
lag_df2<- function( df , laglist ){
    .Call(`_vyos_lag_df_c`, df, laglist)
}
