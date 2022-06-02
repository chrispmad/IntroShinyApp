
title: “Introductory RShiny App” author: “Chris Madsen” date:
“2022-06-02” keep\_md: TRUE

## Introductory R Shiny App

I put together a simple R shiny app to show how one can create shiny
apps of varying complexities. In my mind, there are at least three
levels of complexity:

1.  Basic input and output reactivity (all shiny apps use this)
2.  Using inputs that are variable names inside other (e.g. tidyverse)
    functions
3.  Dynamically rendering UI elements in your app based on inputs the
    user provides

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](README_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
