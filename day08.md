Advent of Code 2017, Day 8
================
Jenny Bryan
2017-12-08

<http://adventofcode.com/2017/day/8>

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

max_register <- function(x) {
  df <- tibble(x = x) %>%
    separate(x, into = c("instruction", "condition"), sep = " if ") %>%
    extract(instruction, into = "i_reg", regex = "(^[a-z]+).*", remove = FALSE) %>%
    extract(condition, into = "c_reg", regex = "(^[a-z]+).*", remove = FALSE) %>%
    mutate(
      instruction = gsub("inc", "+", instruction),
      instruction = gsub("dec", "-", instruction),
      instruction = map2_chr(instruction, i_reg, ~ gsub(.y, paste(.y, "<-", .y), .x)),
      max = 0
    )
  registers <- df %>% select(i_reg, c_reg) %>% unlist() %>% unique() %>% sort()
  e <- new.env(parent = baseenv())
  walk(registers, ~ assign(.x, 0L, envir = e))
  for(i in seq_len(nrow(df))) {
    cond <- eval(parse(text = df$condition[i]), envir = e)
    if (cond) eval(parse(text = df$instruction[i]), envir = e)
    df$max[i] <- max(flatten_dbl(as.list(e)))
  }
  c(max(df$max), df$max[nrow(df)])
}

expect_equal(max_register(read_lines("day08_example.txt")), c(10, 1))
max_register(read_lines("day08_input.txt"))
#> [1] 7184 6343
```
