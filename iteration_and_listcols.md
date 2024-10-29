Iteration and list cols
================

``` r
library(tidyverse)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## Here’s some lists

``` r
l = list(
  vec_numeric = 1:4,
  unif_sample = runif(100),
  mat = matrix(1:8, nrow = 2, ncol = 4, byrow = TRUE),
  summary = summary(rnorm(1000))
)

l
```

    ## $vec_numeric
    ## [1] 1 2 3 4
    ## 
    ## $unif_sample
    ##   [1] 0.158190655 0.963400057 0.255161097 0.566363224 0.554320288 0.703152830
    ##   [7] 0.495539422 0.134979144 0.510675728 0.271415463 0.365453159 0.343827154
    ##  [13] 0.599497977 0.441452455 0.532760678 0.162758888 0.044886788 0.581626613
    ##  [19] 0.405826909 0.295711028 0.774001600 0.409810016 0.961930924 0.190101816
    ##  [25] 0.011058101 0.740954316 0.871273574 0.222292776 0.750744377 0.516288298
    ##  [31] 0.774176336 0.306312193 0.743166681 0.556588392 0.761360063 0.951464863
    ##  [37] 0.006196837 0.051612002 0.642327648 0.910164796 0.109629121 0.394524089
    ##  [43] 0.077836270 0.093651768 0.755439908 0.728858854 0.644327234 0.266522011
    ##  [49] 0.101998780 0.278892487 0.579896777 0.055971756 0.877597558 0.488643881
    ##  [55] 0.099125427 0.906419709 0.590309887 0.378887615 0.515183890 0.275568085
    ##  [61] 0.331378311 0.414128168 0.014677227 0.172049297 0.345358909 0.535489148
    ##  [67] 0.087040788 0.233459215 0.799584905 0.779310455 0.710178181 0.130121424
    ##  [73] 0.563466435 0.822051312 0.012962756 0.810908305 0.858868570 0.300007924
    ##  [79] 0.870121687 0.379551083 0.552138598 0.313105260 0.969199685 0.332077160
    ##  [85] 0.487215661 0.645331533 0.057907097 0.640790717 0.431640406 0.226133657
    ##  [91] 0.396011975 0.467729634 0.127472992 0.548845352 0.809635139 0.008236027
    ##  [97] 0.697322872 0.641460841 0.502732218 0.721894800
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -2.63966 -0.66241  0.07143  0.02814  0.67442  2.96072

``` r
l$mat
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8

``` r
l[["mat"]][1, 3]
```

    ## [1] 3

``` r
l[[1]]
```

    ## [1] 1 2 3 4

``` r
l[[4]]
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -2.63966 -0.66241  0.07143  0.02814  0.67442  2.96072

Make a list that’s hopefully a bit more useful.

``` r
list_norm = 
  list(
    a = rnorm(20, 0, 5),
    b = rnorm(20, 4, 5),
    c = rnorm(20, 0, 10),
    d = rnorm(20, 4, 10)
  )
  
list_norm[["b"]]
```

    ##  [1]  4.4171692  6.0744108  7.1195017  8.3097521 11.5854779 -0.9304335
    ##  [7] 12.9300676 -0.7124599 -4.1762636  5.5825795  4.7700800  2.4305483
    ## [13]  5.7859757 -0.6185919 -3.8757641  6.3217984  6.6206567  5.2723272
    ## [19]  7.2620704 12.1695654

Let’s reuse the function we wrote last time.

``` r
mean_and_sd = function(x) {
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  out_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  
  return(out_df)
  
}
```

Let’s use the function to take mean and sd of all samples.

``` r
mean_and_sd(list_norm[["a"]])
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.375  4.31

``` r
mean_and_sd(list_norm[["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.82  4.89

``` r
mean_and_sd(list_norm[["c"]])
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.501  10.6

``` r
mean_and_sd(list_norm[["d"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.83  8.40

## Use a for loop

Create output list, and run a for loop

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])
  
}

output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.375  4.31
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.82  4.89
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.501  10.6
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.83  8.40

## DO the same thing

but with `map` instead

``` r
output = map(list_norm, mean_and_sd)
```

Let’s do a couple of other things

``` r
output = 
  map(list_norm, mean_and_sd) |>
  bind_rows()

output = map_dfr(list_norm, mean_and_sd) 

output = map_dbl(list_norm, IQR) 
```
