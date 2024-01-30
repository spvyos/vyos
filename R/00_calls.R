
# ---------------------------------------------------------------------------cls
cls <- function(){
  cat("\014")
}
c_all <- function(.test= F ){
  cls()
  clear();
  devtools::load_all() ;
  if(.test)
   devtools::test()
}

#========================================================================= clear
clear <- function() {
  items <-  ls(all.names = TRUE , envir = rlang::global_env())
  items <-
    items[!items %in% c("clear" ,
                        "c_all" ,
                        "success"  ,
                        "load_all" ,
                        "test" ,
                        "true" ,
                        "false" ,
                        "null")]
  print(items)
  rm(list = items , envir = rlang::global_env())
  # gc()
  .green("cleared all items.")
}
