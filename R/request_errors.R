
error_list <- function() {
  nums = c(400 ,  404 ,  423 ,  429 ,  500)
  errors = c("Bad Request" ,
             "Not Found" ,
             "Locked" ,
             "Too Many Requests" ,
             "Internal Server Error")
  df <- list(nums = nums ,
             errors = errors)
  as.data.frame(df)
}
error_means <- function(error_code = 400) {
  error_list() %>% dplyr::filter(nums == error_code) %>%  .$errors
}