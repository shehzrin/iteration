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

    ##  [1]  0.252421373  0.006447509  1.830960857  1.328759819 -0.099657737
    ##  [6]  0.330371122 -2.483903652  1.232780002  1.621544716 -0.837340115
    ## [11] -0.586671367 -1.436346312 -1.330150389 -0.337996672 -0.867079399
    ## [16]  0.387860337 -0.608090847  0.963197155 -0.213149704  0.024238203
    ## [21]  0.659276612  0.015129728 -0.293640246  0.732426069 -0.291387064

Now i’ll write a function to do this.

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z) 
}

z_scores(x = x_vec)
```

    ##  [1]  0.252421373  0.006447509  1.830960857  1.328759819 -0.099657737
    ##  [6]  0.330371122 -2.483903652  1.232780002  1.621544716 -0.837340115
    ## [11] -0.586671367 -1.436346312 -1.330150389 -0.337996672 -0.867079399
    ## [16]  0.387860337 -0.608090847  0.963197155 -0.213149704  0.024238203
    ## [21]  0.659276612  0.015129728 -0.293640246  0.732426069 -0.291387064

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

    ##  [1]  0.252421373  0.006447509  1.830960857  1.328759819 -0.099657737
    ##  [6]  0.330371122 -2.483903652  1.232780002  1.621544716 -0.837340115
    ## [11] -0.586671367 -1.436346312 -1.330150389 -0.337996672 -0.867079399
    ## [16]  0.387860337 -0.608090847  0.963197155 -0.213149704  0.024238203
    ## [21]  0.659276612  0.015129728 -0.293640246  0.732426069 -0.291387064

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

    ##  [1]  0.252421373  0.006447509  1.830960857  1.328759819 -0.099657737
    ##  [6]  0.330371122 -2.483903652  1.232780002  1.621544716 -0.837340115
    ## [11] -0.586671367 -1.436346312 -1.330150389 -0.337996672 -0.867079399
    ## [16]  0.387860337 -0.608090847  0.963197155 -0.213149704  0.024238203
    ## [21]  0.659276612  0.015129728 -0.293640246  0.732426069 -0.291387064

## A new function!

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

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.71  2.55

## Check stuff using a simulation

``` r
sim_df = 
  tibble(
    x = rnorm(30, 10, 5)
  )

sim_df |> 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.08  5.29

Simulation function to check sample mean and sd.

``` r
sim_mean_sd = function(samp_size = 30, true_mean = 10, true_sd = 5) {
  
  sim_df = 
    tibble(
      x = rnorm(samp_size, true_mean, true_sd)
    )

  out_df = 
    sim_df |> 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
  
  return(out_df)
}

sim_mean_sd(samp_size = 30, true_mean = 4, true_sd = 12)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.56  13.0

``` r
sim_mean_sd(true_mean = 4, true_sd = 12, samp_size = 30)
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.131  12.3

``` r
sim_mean_sd(30, 16, 2)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  15.6  2.00

``` r
sim_mean_sd(samp_size = 43)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.81  4.43

## Revisit LoTR words

``` r
fellowship_df = 
  read_excel("data/LotR_Words.xlsx", range = "B3:D6") |> 
  mutate(movie = "fellowship") |> 
  janitor::clean_names()

two_towers_df = 
  read_excel("data/LotR_Words.xlsx", range = "F3:H6") |> 
  mutate(movie = "two_towers") |> 
  janitor::clean_names()

return_king_df = 
  read_excel("data/LotR_Words.xlsx", range = "J3:L6") |> 
  mutate(movie = "return_king") |> 
  janitor::clean_names()
```

Let’s do this using a function instead.

``` r
lotr_import = function(cell_range, movie_title) {
  
  movie_df = 
    read_excel("data/LotR_Words.xlsx", range = cell_range) |> 
    mutate(movie = movie_title) |> 
    janitor::clean_names() |> 
    pivot_longer(
      female:male,
      names_to = "sex",
      values_to = "words"
    ) |> 
    select(movie, everything())
  
  return(movie_df)
  
}

lotr_df = 
  bind_rows(
    lotr_import("B3:D6", "fellowship"),
    lotr_import("F3:H6", "two_towers"),
    lotr_import("J3:L6", "return_king")
  )
```

## NSDUH

don’t do this:

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

marj_table = 
  nsduh_html |> 
  html_table() |> 
  nth(1) |> 
  slice(-1) |> 
  mutate(drug = "marj")

cocaine_table = 
  nsduh_html |> 
  html_table() |> 
  nth(4) |> 
  slice(-1) |> 
  mutate(drug = "cocaine")

heroin_table = 
  nsduh_html |> 
  html_table() |> 
  nth(5) |> 
  slice(-1) |> 
  mutate(drug = "heroin")
```

do this instead:

\`\`{r} source(“source/nsduh_table_format.R”)

bind_rows( nsduh_table_format(html = nsduh_html, 1, “marj”),
nsduh_table_format(html = nsduh_html, 4, “cocaine”),
nsduh_table_format(html = nsduh_html, 5, “heroin”) ) \`\`\`
