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

#' ### Functions
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

#' ### Test input
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

#' ### My input
x <- readLines("day12_input.txt")
disc <- x %>% prep() %>% disconnected()
length(disc)
