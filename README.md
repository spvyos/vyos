
## vyos
[![R-CMD-check](https://github.com/spvyos/vyos/actions/workflows/R_CMD_check.yml/badge.svg)](https://github.com/spvyos/vyos/actions/workflows/R_CMD_check.yml)

## Overview

vyos package is an interface to make requests from data providers. 
Current version is able to connect to APIs of EDDS of CBRT (Central Bank of the Republic of Türkiye)
and FRED API of FED (Federal Reserve Bank). 


## Installation

``` r
# install.packages("vyos")

```


### Development version
``` r
library(devtools)
install_github("spvyos/vyos-repo")
```

## Usage

### set_api_key



``` r
set_api_key( "ABCDEFGHIJKLMOP" , "evds" , "env" )
set_api_key( "ABCDEFGHIJKLMOP" , "fred" , "env" )
# or
set_api_key( "ABCDEFGHIJKLMOP" , "evds" , "file" )
set_api_key( "ABCDEFGHIJKLMOP" , "fred" , "file" )

```

###  get_series 

``` r

    
  obj<- get_series( "UNRATE" )
  print(obj)
  df<- obj$data
  print(df)
# ->[fred]: pausing before a new request.->[fred]: pausing before a new request.
# ======================================vyos_GETPREP=======
#   status      : completed
#   index       : UNRATE
#   start_year  : 2000-01-01
#   end_year    : 2100-01-01
# ................... resolved [completed] ..............
# 
# ..................................
#   .........> lines   .............
# ..................................
# # each line corresponds a different set of func and data
#     data can be reached as below
#         --> obj$lines$data
#   # A tibble: 1 × 8
#   index  source base   comments freq  fnc_str         fnc          data    
#   <chr>  <chr>  <chr>  <chr>    <chr> <chr>           <named list> <list>  
# 1 UNRATE fred   series " "      null  fred_series_fnc <fn>         <tibble>
# ..................................
#     .........> (combined) data ...
# ..................................
#     a combined data frame will be constructed
#     combined data can be reached as
#         --> obj$data
#   # A tibble: 228 × 2
#    date       UNRATE
#    <date>      <dbl>
#  1 2005-01-01    5.3
#  2 2005-02-01    5.4
#  3 2005-03-01    5.2
#  4 2005-04-01    5.2
#  5 2005-05-01    5.1
#  6 2005-06-01    5  
#  7 2005-07-01    5  
#  8 2005-08-01    4.9
#  9 2005-09-01    5  
# 10 2005-10-01    5  
# # ℹ 218 more rows
# # ℹ Use `print(n = ...)` to see more rows
# ...........................................................
# 
# =========================================================
# # A tibble: 228 × 2
#    date       UNRATE
#    <date>      <dbl>
#  1 2005-01-01    5.3
#  2 2005-02-01    5.4
#  3 2005-03-01    5.2
#  4 2005-04-01    5.2
#  5 2005-05-01    5.1
#  6 2005-06-01    5  
#  7 2005-07-01    5  
#  8 2005-08-01    4.9
#  9 2005-09-01    5  
# 10 2005-10-01    5  
# # ℹ 218 more rows
# # ℹ Use `print(n = ...)` to see more rows
  ```

...
``` r

    
  template <- "
  UNRATE 
  bie_abreserv
  "
  obj <- get_series( template )
  print( obj )
  df <- obj$data
  print(df )


> print( obj )

#   
# ======================================vyos_GETPREP=======
#   status      : completed
#   index       : 
#   UNRATE 
#   bie_abreserv
#   
#   start_date  : 2000-01-01
#   end_date    : 2100-01-01
# ................... resolved [completed] ..............
# 
# ..................................
#   .........> lines   .............
# ..................................
# # each line corresponds a different set of func and data
#     data can be reached as below
#         --> obj$lines$data
#   # A tibble: 2 × 8
#   index        source base   comments freq  fnc_str         fnc          data              
#   <chr>        <chr>  <chr>  <chr>    <chr> <chr>           <named list> <list>            
# 1 UNRATE       fred   series " "      null  fred_series_fnc <fn>         <tibble [228 × 2]>
# 2 bie_abreserv evds   table  " "      null  evds_table_fnc  <fn>         <tibble [287 × 6]>
# ..................................
#     .........> (combined) data ...
# ..................................
#     a combined data frame will be constructed
#     combined data can be reached as
#         --> obj$data
#   # A tibble: 287 × 7
#    date       UNRATE TP_AB_B1 TP_AB_B2 TP_AB_B3 TP_AB_B4 TP_AB_B6
#    <date>      <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#  1 2005-01-01    5.3    1592.   37095.   15254.   38687.   53941.
#  2 2005-02-01    5.4    1618.   36652.   15178.   38270.   53448.
#  3 2005-03-01    5.2    1592    38049.   12767.   39641.   52408.
#  4 2005-04-01    5.2    1615.   37334.   13431.   38949.   52380.
#  5 2005-05-01    5.1    1562.   35804.   14924.   37366.   52290.
#  6 2005-06-01    5      1625.   39981.   15968.   41606.   57573.
#  7 2005-07-01    5      1590.   42905.   16266.   44495.   60761.
#  8 2005-08-01    4.9    1618.   41183.   18685.   42800.   61485.
#  9 2005-09-01    5      1764.   41765.   19445.   43529    62974.
# 10 2005-10-01    5      1763.   44811.   14702.   46574    61276.
# # ℹ 277 more rows
# # ℹ Use print(n = ...) to see more rows
# ...........................................................
# 
# =========================================================

```
``` r
  
o <- get_series("bie_yssk" , start_date = "2010-01-01")
o
# ======================================vyos_GETPREP=======
#     status      : completed
# index       : bie_yssk
# start_date  : 2010-01-01
# end_date    : 2100-01-01
# ................... resolved [completed] ..............
# 
# ..................................
# .........> lines   .............
# ..................................
# # each line corresponds to a different set of func and data
# data can be reached as below
> obj$lines$data
# # A tibble: 1 × 8
# index    source base  comments freq  fnc_str        fnc          data              
# <chr>    <chr>  <chr> <chr>    <chr> <chr>          <named list> <list>            
#     1 bie_yssk evds   table " "      null  evds_table_fnc <fn>         <tibble [167 × 7]>
#     ..................................
# .........> (combined) data ...
# ..................................
# a combined data frame will be constructed
# combined data can be reached as
> obj$data
# # A tibble: 167 × 7
# date       TP_YSSK_A1 TP_YSSK_A2 TP_YSSK_A3 TP_YSSK_A4 TP_YSSK_A5 TP_YSSK_A6
# <date>          <dbl>      <dbl>      <dbl>      <dbl>      <dbl>      <dbl>
#     1 2010-01-01       7928       6126       5020       5644      51100      75818
# 2 2010-02-01       7619       6030       4911       5521      50088      74168
# 3 2010-03-01       7517       5998       4920       5534      49625      73595
# 4 2010-04-01       7333       5822       4859       5435      49360      72809
# 5 2010-05-01       7136       5510       4922       5266      48108      70942
# 6 2010-06-01       6906       5257       4449       5277      47464      69353
# 7 2010-07-01       6836       5363       4445       5396      49051      71092
# 8 2010-08-01       6758       5291       4411       5281      48407      70148
# 9 2010-09-01       6799       5200       4411       5375      50099      71885
# 10 2010-10-01       6770       5094       4324       5358      51091      72637
# # ℹ 157 more rows
# # ℹ Use print(n = ...) to see more rows
# ...........................................................
# 
# =========================================================
```
### indexes can be given as a vector or a string template 
```r

index_vector  = c( "TP_YSSK_A1" , "TP_YSSK_A2" )
# or as a template it gives same result 
index_template <- "
TP_YSSK_A1
TP_YSSK_A2
"

o <- get_series(index_vector )
o

o <- get_series(index_template )
o
```
> combined data frame can be accessed via obj$data 
 see also obj$lines
 
```r 
df_raw <- o$data

df_raw
# # A tibble: 287 × 3
# date       TP_YSSK_A1 TP_YSSK_A2
# <date>          <dbl>      <dbl>
#     1 2000-01-01         NA         NA
# 2 2000-02-01         NA         NA
# 3 2000-03-01         NA         NA
# 4 2000-04-01         NA         NA
# 5 2000-05-01         NA         NA
# 6 2000-06-01         NA         NA
# 7 2000-07-01         NA         NA
# 8 2000-08-01         NA         NA
# 9 2000-09-01         NA         NA
# 10 2000-10-01         NA         NA
# # ℹ 277 more rows
# # ℹ Use `print(n = ...)` to see more rows
```

###  remove_na_safe
> removes NA values from the data frame if all the columns are NA to a certain point and after a certain point. When there is any value that is not NA in a row it continues without removing any row until all the columns of a row is NA again to protect the meaningful date sequence of the time series of the current data frame.  

```r 
df <- remove_na_safe(df_raw )
df 
# # A tibble: 263 × 3
# date       TP_YSSK_A1 TP_YSSK_A2
# <date>          <dbl>      <dbl>
#     1 2002-01-01       2673       1197
# 2 2002-02-01       3235       1262
# 3 2002-03-01       3561       1432
# 4 2002-04-01       3872       1525
# 5 2002-05-01       4124       1642
# 6 2002-06-01       4432       1748
# 7 2002-07-01       4823       1841
# 8 2002-08-01       4903       1732
# 9 2002-09-01       5155       1706
# 10 2002-10-01       5066       1709
# # ℹ 253 more rows
# ℹ Use `print(n = ...)` to see more rows

```

### lag_df  

> it takes a data frame and a list of column names and lag sequences, it creates a data frame with lagged values 
of the column names given if the column name exists in data frame. 

```r
df2 <- lag_df( df , list( TP_YSSK_A1 = 1 : 3 , TP_YSSK_A2 = 1 : 6 ) )
df2
# # A tibble: 263 × 12
# date       TP_YSSK_A1 TP_YSSK_A2 TP_YSSK_A1_lag_1 TP_YSSK_A1_lag_2 TP_YSSK_A1_lag_3 TP_YSSK_A2_lag_1 TP_YSSK_A2_lag_2
# <date>          <dbl>      <dbl>            <dbl>            <dbl>            <dbl>            <dbl>            <dbl>
#     1 2002-01-01       2673       1197               NA               NA               NA               NA               NA
# 2 2002-02-01       3235       1262             2673               NA               NA             1197               NA
# 3 2002-03-01       3561       1432             3235             2673               NA             1262             1197
# 4 2002-04-01       3872       1525             3561             3235             2673             1432             1262
# 5 2002-05-01       4124       1642             3872             3561             3235             1525             1432
# 6 2002-06-01       4432       1748             4124             3872             3561             1642             1525
# 7 2002-07-01       4823       1841             4432             4124             3872             1748             1642
# 8 2002-08-01       4903       1732             4823             4432             4124             1841             1748
# 9 2002-09-01       5155       1706             4903             4823             4432             1732             1841
# 10 2002-10-01       5066       1709             5155             4903             4823             1706             1732
# # ℹ 253 more rows
# # ℹ 4 more variables: TP_YSSK_A2_lag_3 <dbl>, TP_YSSK_A2_lag_4 <dbl>, TP_YSSK_A2_lag_5 <dbl>, TP_YSSK_A2_lag_6 <dbl>
# # ℹ Use `print(n = ...)` to see more rows
```

> Does not require to give source name. it figures out from the index IDs given.

```r 
index_template <- "
TP_YSSK_A1
TP_YSSK_A2
UNRATE
"

o <- get_series(index_template )
o

# [cache was saved]->[evds]: pausing before a new request.
# [cache was saved]->[fred]: pausing before a new request.> o
# 
# ======================================vyos_GETPREP=======
#     status      : completed
# index       : 
#     TP_YSSK_A1
# TP_YSSK_A2
# UNRATE
# 
# start_year  : 2000-01-01
# end_year    : 2100-01-01
# ................... resolved [completed] ..............
# 
# ..................................
# .........> lines   .............
# ..................................
# # each line corresponds to a different set of func and data
# data can be reached as below
# --> obj$lines$data
# # A tibble: 3 × 8
# index      source base   comments freq  fnc_str         fnc          data              
# <chr>      <chr>  <chr>  <chr>    <chr> <chr>           <named list> <list>            
#     1 TP_YSSK_A1 evds   series " "      null  evds_series_fnc <fn>         <tibble [287 × 2]>
#     2 TP_YSSK_A2 evds   series " "      null  evds_series_fnc <fn>         <tibble [287 × 2]>
#     3 UNRATE     fred   series " "      null  fred_series_fnc <fn>         <tibble [228 × 2]>
#     ..................................
# .........> (combined) data ...
# ..................................
# a combined data frame will be constructed
# combined data can be reached as
# --> obj$data
# # A tibble: 228 × 4
# date       TP_YSSK_A1 TP_YSSK_A2 UNRATE
# <date>          <dbl>      <dbl>  <dbl>
#     1 2005-01-01       5509       2226    5.3
# 2 2005-02-01       5581       2299    5.4
# 3 2005-03-01       5507       2347    5.2
# 4 2005-04-01       5699       2444    5.2
# 5 2005-05-01       5802       2404    5.1
# 6 2005-06-01       6023       2321    5  
# 7 2005-07-01       5886       2565    5  
# 8 2005-08-01       6079       2577    4.9
# 9 2005-09-01       5986       2525    5  
# 10 2005-10-01       6103       2548    5  
# # ℹ 218 more rows
# # ℹ Use `print(n = ...)` to see more rows
# ...........................................................
# 
# =========================================================

```
> individual data frames can be reached via object$lines$data 

```r
> o$lines
# # A tibble: 3 × 8
#   index        source base   comments      freq  fnc_str         fnc          data              
#   <chr>        <chr>  <chr>  <chr>         <chr> <chr>           <named list> <list>            
# 1 UNRATE       fred   series fred (series) null  fred_series_fnc <fn>         <tibble [228 × 2]>
# 2 bie_abreserv evds   table  evds (table)  null  evds_table_fnc  <fn>         <tibble [287 × 6]>
# 3 TP.AB.B1     evds   series evds (series) null  evds_series_fnc <fn>         <tibble [287 × 2]>
> o$lines$data
# [[1]]
# # A tibble: 228 × 2
#    date       UNRATE
#    <date>      <dbl>
#  1 2005-01-01    5.3
#  2 2005-02-01    5.4
#  3 2005-03-01    5.2
#  4 2005-04-01    5.2
#  5 2005-05-01    5.1
#  6 2005-06-01    5  
#  7 2005-07-01    5  
#  8 2005-08-01    4.9
#  9 2005-09-01    5  
# 10 2005-10-01    5  
# # ℹ 218 more rows
# # ℹ Use `print(n = ...)` to see more rows
# 
# [[2]]
# # A tibble: 287 × 6
#    date       TP_AB_B1 TP_AB_B2 TP_AB_B3 TP_AB_B4 TP_AB_B6
#    <date>        <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#  1 2000-01-01    1011    22859.    8943.   23870.   32812.
#  2 2000-02-01    1011    22907.    8296.   23918.   32214.
#  3 2000-03-01    1011.   22926.    9817.   23937.   33754.
#  4 2000-04-01    1011.   22337     8579.   23348.   31926.
#  5 2000-05-01    1011.   22950.    8451.   23961.   32412.
#  6 2000-06-01    1011.   24547.    9270.   25558.   34827.
#  7 2000-07-01    1010.   24477.   10575.   25487    36062.
#  8 2000-08-01    1033    24457    10146.   25490    35636.
#  9 2000-09-01    1025    24160    10715.   25185    35900.
# 10 2000-10-01     988    23593     9970.   24581    34551.
# # ℹ 277 more rows
# # ℹ Use `print(n = ...)` to see more rows
# 
# [[3]]
# # A tibble: 287 × 2
#    date       TP.AB.B1
#    <date>        <dbl>
#  1 2000-01-01    1011 
#  2 2000-02-01    1011 
#  3 2000-03-01    1011.
#  4 2000-04-01    1011.
#  5 2000-05-01    1011.
#  6 2000-06-01    1011.
#  7 2000-07-01    1010.
#  8 2000-08-01    1033 
#  9 2000-09-01    1025 
# 10 2000-10-01     988 
# # ℹ 277 more rows
# # ℹ Use `print(n = ...)` to see more rows
```

### excel 

> excel function to write all data frames of the object 


```r
> obj <- get_series( index = template_test() )
> excel(obj , "file_name.xlsx" , "somefolder" ) 
or 
> excel(obj , "file_name" , "somefolder" ) 

```
## Getting api keys

## CBRT

https://evds2.tcmb.gov.tr/index.php?/evds/userDocs

    
## FRED 

https://fred.stlouisfed.org/docs/api/api_key.html


    

