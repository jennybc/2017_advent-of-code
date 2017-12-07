#' ---
#' title: "Advent of Code 2017, Day 7"
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

#' <http://adventofcode.com/2017/day/7>

library(tidyverse)

## part 1

bottom <- function(x) {
  tibble(x = x) %>%
    separate(x, into = c("name_weight", "above"), sep = " -> ", fill = "right") %>%
    separate(name_weight, into = c("name", "weight"), extra = "drop") %>%
    separate_rows(above, sep = ", ") %>%
    filter(!is.na(above)) %>% {
      setdiff(.$name, .$above)
    }
}

testthat::expect_equal(bottom(read_lines("day07_part1_example.txt")), "tknk")
bottom(read_lines("day07_input.txt"))

## part 2
x <- read_lines("day07_input.txt")

df <- tibble(x = x) %>%
  separate(x, into = c("name_weight", "above"), sep = " -> ", fill = "right") %>%
  separate(name_weight, into = c("name", "weight"), extra = "drop") %>%
  mutate(
    weight = as.integer(weight),
    above = strsplit(above, ", ")
  )

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
oops <- z != min(z)

## it's this one
z[oops]

## this is what its weight needs to be for balance
min(z) - sum(res[[1]])
