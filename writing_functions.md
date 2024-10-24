Writing Functions
================

Load key packages.

``` r
library(tidyverse)
library(readxl)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## writing my first function!!

as an example, here’s a z-score computation

``` r
x_vec = rnorm(n = 25, mean = 10, sd = 3.5)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.15996516 -0.41829966  1.93812391 -0.72416529 -1.24444542 -0.25549425
    ##  [7] -1.43652594 -1.24758504  0.02175410 -0.46746523 -1.01170705 -0.17659928
    ## [13] -0.97099773 -0.70717676  0.48679786  1.12013494  1.38611787  1.58272001
    ## [19]  1.51773687  0.07122701  0.58898693 -1.16566262  1.23394199 -0.21779133
    ## [25]  0.25633927

Now i’ll write a function to do this.

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z) 
}

z_scores(x = x_vec)
```

    ##  [1] -0.15996516 -0.41829966  1.93812391 -0.72416529 -1.24444542 -0.25549425
    ##  [7] -1.43652594 -1.24758504  0.02175410 -0.46746523 -1.01170705 -0.17659928
    ## [13] -0.97099773 -0.70717676  0.48679786  1.12013494  1.38611787  1.58272001
    ## [19]  1.51773687  0.07122701  0.58898693 -1.16566262  1.23394199 -0.21779133
    ## [25]  0.25633927

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 5) {
    stop("you need at least five numbers to compute the z score")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1] -0.15996516 -0.41829966  1.93812391 -0.72416529 -1.24444542 -0.25549425
    ##  [7] -1.43652594 -1.24758504  0.02175410 -0.46746523 -1.01170705 -0.17659928
    ## [13] -0.97099773 -0.70717676  0.48679786  1.12013494  1.38611787  1.58272001
    ## [19]  1.51773687  0.07122701  0.58898693 -1.16566262  1.23394199 -0.21779133
    ## [25]  0.25633927

does this always work?

``` r
z_scores(x = 3)
```

    ## Error in z_scores(x = 3): you need at least five numbers to compute the z score

``` r
z_scores(x = c("my", "name", "is", "jeff"))
```

    ## Error in z_scores(x = c("my", "name", "is", "jeff")): x needs to be numeric

``` r
z_scores(x = x_vec)
```

    ##  [1] -0.15996516 -0.41829966  1.93812391 -0.72416529 -1.24444542 -0.25549425
    ##  [7] -1.43652594 -1.24758504  0.02175410 -0.46746523 -1.01170705 -0.17659928
    ## [13] -0.97099773 -0.70717676  0.48679786  1.12013494  1.38611787  1.58272001
    ## [19]  1.51773687  0.07122701  0.58898693 -1.16566262  1.23394199 -0.21779133
    ## [25]  0.25633927
