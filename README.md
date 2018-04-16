
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kwm

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

predict(month_model, newdata = month_df)
#>   January  February     March     April       May      June      July 
#>      TRUE      TRUE      TRUE      TRUE      TRUE      TRUE     FALSE 
#>    August September   October  November  December 
#>      TRUE     FALSE     FALSE     FALSE     FALSE
```
