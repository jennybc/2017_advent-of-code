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

Part 1
------

### Functions

``` r
## x: file path or character string containing newlines
## returns tibble with variables `depth` and `range`, each describing
## one scanner
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

## severity of a trip with `delay` through firewall described by df
severity <- function(df, delay = 0) {
  df %>%
    mutate(sp = map2_dbl(depth + 1 + delay, range, scan_pos),
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

Part 2, a SLOW but correct version in R
---------------------------------------

### Functions

Now we care about getting caught or not, not about severity.

``` r
## returns a logical indicating if you get caught on trip with `delay` through
## firewall described by `df`
caught <- function(df, delay = 0) {
  df %>%
    mutate(sp = map2_dbl(depth + 1 + delay, range, scan_pos),
           caught = sp == 1) %>%
    pull(caught) %>%
    any()
}
```

### Test input

``` r
x <- "0: 3\n1: 2\n4: 4\n6: 4"
expect_true(all(map_lgl(0:9, ~ prep(x) %>% caught(delay = .x))))
expect_false(caught(prep(x), delay = 10))
```

### My input

``` r
(df <- prep("day13_input.txt"))
#> # A tibble: 43 x 2
#>    depth range
#>  * <int> <int>
#>  1     0     4
#>  2     1     2
#>  3     2     3
#>  4     4     4
#>  5     6     6
#>  6     8     5
#>  7    10     6
#>  8    12     6
#>  9    14     6
#> 10    16    12
#> # ... with 33 more rows
```

DO NOT TRY THIS AT HOME! Incredibly slow, but it did allow me to submit a me correct answer. Took hours (I was sleeping).

``` r
# delay <- 0
# system.time(
#   while (caught(df, delay)) {
#     delay <- delay + 1
#     if (delay %% 10000 == 0) cat(delay, "\n")
#   }
# )
# delay
## 3943252

## checking my alleged answer and, as comforting anecdote, previous delay
caught(df, delay = 3943252)
#> [1] FALSE
caught(df, delay = 3943252 - 1)
#> [1] TRUE
```

Part 2, a FAST solution using C++
---------------------------------

This seems like a good excuse to practice putting the time-consuming loop in C++ and calling from R. Here we go!

### Functions

I'll still prep the input in R. Otherwise, do everything in C++.

``` r
Rcpp::sourceCpp("day13.cpp")
```

Signature of main exported function I plan to call (others exported for testing purposes):

`int minDelay(IntegerVector depth, IntegerVector range) {...}`

Inputs `depth` and `range` will be the variables in my usual prepared data frame, describing the firewall. Return value is the minimum delay that allows you to pass through uncaught.

### Testing

``` r
x <- "0: 3\n1: 2\n4: 4\n6: 4"
df <- prep(x)
expect_equal(minDelay(df$depth, df$range), 10)
```

### My input

``` r
df <- prep("day13_input.txt")

## comforting anecdotal exploration around my known solution
map_lgl(3943252 + -2:2, ~ caughtCpp(df$depth, df$range, delay = .x))
#> [1]  TRUE  TRUE FALSE  TRUE  TRUE


## doing it for real, and timing it ... about ~1000x faster!
system.time(
  print(minDelay(df$depth, df$range))
)
#> [1] 3943252
#>    user  system elapsed 
#>   0.382   0.005   0.386
```
