#' ---
#' title: "Advent of Code 2017, Day 11"
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

#' <http://adventofcode.com/2017/day/11>

library(tidyverse)
library(testthat)

#' ### Functions

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

#' ### Example input
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
hdist(x) # h = 438, v = 577
steps(x) # 796
