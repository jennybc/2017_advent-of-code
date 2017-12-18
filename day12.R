#' ---
#' title: "Advent of Code 2017, Day 12"
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

#' <http://adventofcode.com/2017/day/12>

library(tidyverse)
library(testthat)
library(igraph)

#' igraph makes this so easy, it feels like cheating.

prep <- function(s) {
  tibble(x = read_lines(s)) %>%
    separate(x, into = c("a", "b"), sep = " <-> ") %>%
    mutate(
      a = as.integer(a),
      b = map(b, ~ strsplit(.x, split = ", ") %>% pluck(1) %>% as.integer())
    ) %>%
    unnest()
}

#' ## Testing
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

#' ## My input
df <- prep("day12_input.txt")
g <- graph_from_data_frame(df, directed = FALSE)
cpts <- components(g, mode = "weak")

sum(cpts$membership == cpts$membership["0"])

cpts$no
