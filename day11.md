Advent of Code 2017, Day 11
================
Jenny Bryan
2017-12-15

<http://adventofcode.com/2017/day/11>

``` r
library(tidyverse)
library(testthat)
#> 
#> Attaching package: 'testthat'
#> The following object is masked from 'package:dplyr':
#> 
#>     matches
#> The following object is masked from 'package:purrr':
#> 
#>     is_null
```

Part 1
------

### Functions

``` r
## x = character vector of values from n, ne, se, s, sw, nw
## returns 2-vector = c(h = horizontal offset, v = vertical offset)
hdist <- function(x) {
  h <- c(n = 0, ne = 1, se = 1, s = 0, sw = -1, nw = -1)
  v <- c(n = 1, ne = 0.5, se = -0.5, s = -1, sw = -0.5, nw = 0.5)
  c(h = abs(sum(h[x])), v = abs(sum(v[x])))
}

## x = character vector of values from n, ne, se, s, sw, nw
## returns minimum steps needed to return to origin
steps <- function(x) {
  d <- hdist(x)
  unname(0.5 * d["h"] + d["v"])
}
```

### Example input

``` r
t <- tribble(
             ~ raw, ~ ref,
        "ne,ne,ne",     3,
     "ne,ne,sw,sw",     0,
       "ne,ne,s,s",     2,
  "se,sw,se,sw,sw",     3
)
t <- t %>%
  mutate(
    x = map(raw, ~ strsplit(.x, split = ",")[[1]]),
    steps = map_dbl(x, steps)
  )
expect_equal(t$ref, t$steps)

x <- scan("day11_input.txt", what = "", sep = ",")
hdist(x)
#>   h   v 
#> 438 577
steps(x)
#> [1] 796
```

Part 2
------

``` r
x <- scan("day11_input.txt", what = "", sep = ",")

## this is very inefficient, but I'm not starting over!
out <- map_dbl(seq_along(x), ~ steps(head(x, .x)))

## this better be same as previous result!
out[length(out)]
#> [1] 796

max(out)
#> [1] 1585
```
