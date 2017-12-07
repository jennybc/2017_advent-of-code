Advent of Code 2017, Day 7
================
Jenny Bryan
2017-12-07

<http://adventofcode.com/2017/day/7>

``` r
library(tidyverse)

## part 1

bottom <- function(x) {
  suppressWarnings(
    tibble(x = x) %>%
      separate(x, into = c("name_weight", "above"), sep = " -> ") %>%
      separate(name_weight, into = c("name", "weight")) %>%
      separate_rows(above, sep = ", ") %>%
      filter(!is.na(above)) %>% {
        setdiff(.$name, .$above)
      }
  )
}

testthat::expect_equal(bottom(read_lines("day07_part1_example.txt")), "tknk")
bottom(read_lines("day07_input.txt"))
#> [1] "hlhomy"

## part 2
x <- read_lines("day07_input.txt")

df <- tibble(x = x) %>%
  separate(x, into = c("name_weight", "above"), sep = " -> ", fill = "right") %>%
  separate(name_weight, into = c("name", "weight")) %>%
  mutate(
    weight = as.integer(weight),
    above = strsplit(above, ", ")
  )
#> Warning: Too many values at 1605 locations: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
#> 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...

tw <- function(nm) {
  i <- which(df$name == nm)
  wt <- df$weight[i]
  a <- df$above[[i]]
  if (all(is.na(a))) {
    wt
  } else {
    sum(c(unlist(map(a, tw)), wt))
  }
}

df$total_weight <- map_int(df$name, tw)

b <- "hlhomy"
res <- list()
while(TRUE) {
  i <- which(df$name == b)
  a <- df$above[[i]]
  tws <- map_int(a, tw)
  oops <- tws != min(tws)
  res[[b]] <- set_names(tws, a)
  if (!any(oops)) break
  b <- a[oops]
}
res <- rev(res)

## the problematic program is in here
(z <- res[[2]])
#> apjxafk  jngcap wrtyhxg  hblcbb 
#>    1579    1571    1571    1571
oops <- z != min(z)

## it's this one
z[oops]
#> apjxafk 
#>    1579

## this is what its weight needs to be for balance
min(z) - sum(res[[1]])
#> [1] 1505
```
