Advent of Code 2017, Day 12
================
Jenny Bryan
2017-12-16

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
```

### Functions

``` r
prep <- function(s) {
  tibble(x = s) %>%
    separate(x, into = c("a", "b"), sep = " <-> ") %>%
    mutate(
      a = as.integer(a),
      d = map(b, ~ strsplit(.x, split = ", ") %>% pluck(1) %>% as.integer())
    )
}

connected <- function(df, x = 0) {
  for(i in seq_len(nrow(df))) {
    conn <- c(df$a[i], df$d[[i]])
    if (any(x %in% conn)) {
      x <- union(x, conn)
    }
  }
  sort(x)
}

disconnected <- function(df, x = 0) {
  conn <- connected(df, x)
  setdiff(df$a, conn)
}
```

### Test input

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

x %>% prep() %>% disconnected()
#> [1] 1
```

### My input

``` r
x <- readLines("day12_input.txt")
disc <- x %>% prep() %>% disconnected()
length(disc)
#> [1] 1991
```
