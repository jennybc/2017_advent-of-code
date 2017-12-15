#' ---
#' title: "Advent of Code 2017, Day 13"
#' author: "Jenny Bryan"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#+ setup, include = FALSE, cache = FALSE
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  error = TRUE
)
options(tidyverse.quiet = TRUE)

#' <http://adventofcode.com/2017/day/13>

library(tidyverse)
library(testthat)

#' ### Functions
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

#' ### Test input
x <- "0: 3\n1: 2\n4: 4\n6: 4"
expect_equal(x %>% prep() %>% severity(), 24)

#' ### My input
"day13_input.txt" %>% prep() %>% severity()
