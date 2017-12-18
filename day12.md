Advent of Code 2017, Day 12
================
Jenny Bryan
2017-12-17

<http://adventofcode.com/2017/day/12>

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
library(igraph)
#> 
#> Attaching package: 'igraph'
#> The following object is masked from 'package:testthat':
#> 
#>     compare
#> The following objects are masked from 'package:dplyr':
#> 
#>     as_data_frame, groups, union
#> The following objects are masked from 'package:purrr':
#> 
#>     compose, simplify
#> The following object is masked from 'package:tidyr':
#> 
#>     crossing
#> The following object is masked from 'package:tibble':
#> 
#>     as_data_frame
#> The following objects are masked from 'package:stats':
#> 
#>     decompose, spectrum
#> The following object is masked from 'package:base':
#> 
#>     union
```

igraph makes this so easy, it feels like cheating.

``` r
prep <- function(s) {
  tibble(x = read_lines(s)) %>%
    separate(x, into = c("a", "b"), sep = " <-> ") %>%
    mutate(
      a = as.integer(a),
      b = map(b, ~ strsplit(.x, split = ", ") %>% pluck(1) %>% as.integer())
    ) %>%
    unnest()
}
```

Testing
-------

``` r
x <- c(
  "0 <-> 2",
  "1 <-> 1",
  "2 <-> 0, 3, 4",
  "3 <-> 2, 4",
  "4 <-> 2, 3, 6",
  "5 <-> 6",
  "6 <-> 4, 5"
)

df <- prep(x)
g <- graph_from_data_frame(df, directed = FALSE)
cpts <- components(g)
expect_equal(sum(cpts$membership == cpts$membership["0"]), 6)
expect_equal(cpts$no, 2)
```

My input
--------

``` r
df <- prep("day12_input.txt")
g <- graph_from_data_frame(df, directed = FALSE)
cpts <- components(g, mode = "weak")

sum(cpts$membership == cpts$membership["0"])
#> [1] 141

cpts$no
#> [1] 171
```
