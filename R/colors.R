
#=================================================================
#                   color functions
#                   crayon + glue => .blue(" pi number is { pi }")
#=================================================================
color_fnc_create <-function(color = crayon::green ){
  function(...){
    x <- glue::glue( ... , .envir=  rlang::caller_env()   )
    cat(color( x ))
  }
}
.green   <- color_fnc_create(crayon::green );    .red     <- color_fnc_create(crayon::red )
.bold    <- color_fnc_create(crayon::bold );     .cyan    <- color_fnc_create(crayon::cyan )
.yellow  <- color_fnc_create(crayon::yellow );   .blue    <- color_fnc_create(crayon::blue )
.magenta <- color_fnc_create(crayon::magenta );  .silver  <- color_fnc_create(crayon::silver )
test_colors <- function(){
  pid_ <- 15465L
  msg<- "...pid -> { pid_ } \n\r\n\r"
  cat("\n\r Testing colors ...\n\r\n\r")
  color_fncs <- c(.bold ,.cyan ,.blue  , .yellow ,.green )
  for (.f in color_fncs ){
    .f( formatC( msg , width= 50 ) )
  }
}