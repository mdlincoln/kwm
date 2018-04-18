
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kwm

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis-CI Build
Status](https://travis-ci.org/mdlincoln/kwm.svg?branch=master)](https://travis-ci.org/mdlincoln/kwm)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/mdlincoln/kwm?branch=master&svg=true)](https://ci.appveyor.com/project/mdlincoln/kwm)

kwm provides very simiple wrapper functions to produce **K**ey**W**ord
**M**odels that produce classification predictions based on explicit
lists of regular expression pattern matches. By supplying a generic
prediction function for such lists, it is easy to compare the
performance of very simple regex matching to other, more complicated
text classification models within the same pipeline.

## Installation

You can install kwm from github with:

``` r
# install.packages("devtools")
devtools::install_github("mdlincoln/kwm")
```

## Example

``` r
library(kwm)

month_df <- data.frame(month = month.name, stringsAsFactors = FALSE)

# Locate all matches that INCLUDE either "a" or "e" but EXCLUDE any ending in "r"
month_model <- kwm(include = c("a", "e"), exclude = "r$", varname = "month")

predict(month_model, newdata = month_df, return_names = TRUE)
#>   January  February     March     April       May      June      July 
#>      TRUE      TRUE      TRUE     FALSE      TRUE      TRUE     FALSE 
#>    August September   October  November  December 
#>     FALSE     FALSE     FALSE     FALSE     FALSE

# You can pass options to the underlying search function as well
caseless_month_model <- kwm(include = c("a", "e"), exclude = "r$", 
                            varname = "month", 
                            search_opts = list(ignore_case = TRUE))

predict(caseless_month_model, newdata = month_df, return_names = TRUE)
#>   January  February     March     April       May      June      July 
#>      TRUE      TRUE      TRUE      TRUE      TRUE      TRUE     FALSE 
#>    August September   October  November  December 
#>      TRUE     FALSE     FALSE     FALSE     FALSE
```
