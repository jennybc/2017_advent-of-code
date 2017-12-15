Advent of Code 2017, Day 13
================
Jenny Bryan
2017-12-16

<http://adventofcode.com/2017/day/13>

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

### Functions

``` r
prep <- function(x) {
  tibble(x = read_lines(x)) %>%
    separate(x, into = c("depth", "range"), sep = ": ", convert = TRUE)
}

## position of scanner with range rg at step i
scan_pos <- function(i, rg = 0) {
  if (rg == 0) return(0)
  x <- c(1:rg, (rg - 1):2)
  nn <- 2 * rg - 2
  x[(i - 1) %% nn + 1]
}

severity <- function(df) {
  df %>%
    mutate(sp = map2_dbl(depth + 1, range, scan_pos),
           severity = ifelse(sp == 1, depth * range, 0)) %>%
    pull(severity) %>%
    sum()
}
```

### Test input

``` r
x <- "0: 3\n1: 2\n4: 4\n6: 4"
expect_equal(x %>% prep() %>% severity(), 24)
```

### My input

``` r
"day13_input.txt" %>% prep() %>% severity()
#> [1] 1580
```
