

#' inn
#' checks if the second parameter includes the first one as a value or a column name or a name
#' @param x character to check if it exists in a vector or list
#' @param table list, data frame or any vector
#'
#' @return bool value TRUE if it exists FALSE if it does not
#' @export
#'
#' @examples
#'  .check = inn( "a" , list( a = 1 : 5 ) )
inn <- function(x , table ){
    if( is.character( table )  || is.numeric( table )){
        return( match(x, table, nomatch = 0L) > 0L)
    }
    if(is.data.frame( table )){
        return ( x   %in%  colnames(table) )
    }
    if(is.list( table )){
        return ( x   %in%  names(table) )
    }
}



#' %inn%
#'
#' @param x character to check if it exists in a vector or list
#' @param table list, data frame or any vector
#'
#' @return bool value TRUE if it exists FALSE if it does not
#' @export
#'
#' @examples
#' .check = "a" %inn% data.frame( a = 1 : 5 )
"%inn%" <- function(x, table) {

    inn( x , table )
}
