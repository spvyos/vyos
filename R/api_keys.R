
api_key_name_format <- function(source_name= "evds"){
   paste0(source_name , "__APIKEY")
}
format_message_set_api_key<-function(source_name="evds"){
  msg = glue::glue('\n
============================================================
  # usage example
  set_api_key( "xyz123456789" ,  "{source_name}"  )
============================================================\n
                   \n')
}
check_api_key_works<- function(source_name = "evds" ,
                               key = ".." ){
  liste <- list( evds = quick_check_evds ,
                 fred = quick_check_fred )
  fnc = liste[[ source_name ]]
  g = glue::glue
  if(is.function(fnc)){
    res = fnc( key )
    if(res )
      success(g("\n
=========================================================
    api key for [{source_name}] was tested successfully.
=========================================================\n
                "))
    return (inv( true ))
  }
  message_api_key_fails(source_name , key )
  Sys.sleep(2)
}
message_api_key_fails <- function(source_name = "evds" ,
                                  key = ".."){
  g = glue::glue
  msg = "\n\r
=========================================================
                  api key `{key}`  for [{source_name}] does NOT work.
                  Check your api key for and set
                    {format_message_set_api_key(source_name)}
=========================================================\n\r"
  message(g(msg ))
}
replace_if_null<-function( x , value ){
  if(is.null(x))
    return (value )
  x
}
show_usage<-function(func_name = "set_api_key" ,
                     source_name = "evds" ,
                     call. = ".."  ){
   .blue( "
   ======================================
          {call.}
          call has an error
   ======================================
          " )
  source_name =  replace_if_null(source_name , "evds" )
  msg =  format_message_set_api_key( source_name )
  message(msg )
}


#' set_api_key
#'
#' @param key api key of the source
#' @param source_name evds or fred
#' @param option choice of later usage. env or file should be given to
#' save api key for later use. Default is env which
#' saves api key as environment variable.
#' if `env` default value is selected it will save api key
#' as an environment variable
#' if `file` was selected it will save api key to current folder.
#' @param ... for future versions
#' @return null
#' @export
#'
#' @examples
#' \dontrun{
#'
#' set_api_key( "ABCDEFGHIJKLMOP" , "evds" , "env" )
#' set_api_key( "ABCDEFGHIJKLMOP" , "fred" , "env" )
#' set_api_key( "ABCDEFGHIJKLMOP" , "fred" , "file" )
#' }
set_api_key<-function(key = null  ,
                      source_name = null   ,
                      option = c("env" , "file")   ,
                      ...  ){
  if(is.null(key) || is.null( source_name )){
    show_usage( "set_api_key" ,
                source_name ,
                deparse(match.call() ) )
    stop()
  }
   # check_required(key)
    option = match.arg(option )
  g = glue::glue
  check_api_key_works(source_name  , key )
  if( identical( option , "env" )  ){
    key_name <-api_key_name_format(source_name  )
    if( is_( source_name , "evds"  ) ){
      Sys.setenv("evds__APIKEY" = toString(key))
    }else{
      Sys.setenv("fred__APIKEY" = toString(key))
    }
    # .Internal(Sys.setenv(key_name,  toString(key)))
    via<- g("
    by setting environment variable with key `{key_name}`
    ")
    success( g("
          APIKEY for [{source_name}] was set {via}
                      ") )
  }

  if(  identical( option , "file" )  ){
    file_name = default_file_name_for_api_keys(source_name)
    key_name <-api_key_name_format(source_name  )
    folder_api_key <- get_folder_api_key()
    create_dir_if_not( folder_api_key  )
    content <- glue::glue( "{key_name}={key}\n\r")
    cat( content , file = file.path( folder_api_key ,  file_name) )
    via<- g("
    by creating a file called `{file_name}`
    ")
    success( g("
          APIKEY for [{source_name}] was set {via}
                      ") )
  }


}
key_looks_okay_general <- function(value ){
  is.character(value ) && nchar(value ) > 5
}
is_a_char <-function(smt ){
  is.character(smt )
}
get_api_key_from_file<-function(source_name){
  file_name = default_file_name_for_api_keys(source_name)
  folder_api_key <- get_folder_api_key()
  file_name_full  <- file.path(folder_api_key , file_name  )
  if(! file.exists(file_name_full ) ) return (F )
  content = read(file_name_full )
  value =  stringr::str_split_1(content  , "=")[[2 ]]
  value <- sp_trim(value)
  value <-  gsubs( c( "\n" , "\r" ) , "" , value )
  inv( value )
}
get_folder_api_key <- function(){
    file.path("APIKEYS")
}
get_api_key<-function(source_name = "evds"){
  key_name <-api_key_name_format(source_name  )
  # env
  value =  Sys.getenv( key_name  )
  if ( key_looks_okay_general(value ) ) return (value )
  # file
  value = get_api_key_from_file(source_name )
  if( key_looks_okay_general(value)) return(value )
  message_api_key( source_name )
}
check_api_key_was_set<-function(source_name = "evds"){
  key_looks_okay_general( get_api_key(source_name ) )
}
default_file_name_for_api_keys<-function(source_name= "evds"){
  file_name = sprintf("KEEP-IT-SECRET-APIKEY-%s.txt" , source_name)
  file_name
}
