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
example_input <- read_lines("day07_part1_example.txt")
jenny_input <-  read_lines("day07_input.txt")

## converts AoC input into a tibble, with one row per program and variables
##   * name
##   * weight
##   * above: a list-column of character vectors of names of the programs above
##            this one; NA indicates no programs are above
##   * total_weight: includes this program and those above; equals weight if
##            above is NA, i.e. there are no programs above
entibble <- function(x) {
  ret <- tibble(x = x) %>%
    separate(x, into = c("name_weight", "above"), sep = " -> ", fill = "right") %>%
    separate(name_weight, into = c("name", "weight"), extra = "drop") %>%
    mutate(
      weight = as.integer(weight),
      above = strsplit(above, ", ")
    )
  ret$total_weight <- map_int(ret$name, tw, df = ret)
  ret
}

## input as given by advent of code
## returns name of program at base of tower
find_root <- function(x) {
  xdf <- entibble(x)
  setdiff(xdf$name, unlist(xdf$above))
}

## compute total weight of program named 'nm'
tw <- function(nm, df) {
  i <- which(df$name == nm)
  wt <- df$weight[i]
  a <- df$above[[i]]
  if (all(is.na(a))) {
    wt
  } else {
    sum(c(unlist(map(a, tw, df)), wt))
  }
}

## part 1
testthat::expect_equal(find_root(example_input), "tknk")
find_root(jenny_input)

## part 2
find_unbalanced <- function(nm, df) {
  i <- which(df$name == nm)
  a <- df$above[[i]]
  tws <- df$total_weight[match(a, df$name)]
  unbalanced <- tws != min(tws)
  if (!any(unbalanced)) {
    return()
  } else {
    ret <- list()
    ret[[nm]] <- set_names(tws, a)
    c(ret, find_unbalanced(a[unbalanced], df))
  }
}

suggest_fix <- function(x) {
  xdf <- entibble(x)
  res <- find_unbalanced(find_root(x), xdf)

  bad_layer <- res[[length(res)]]
  culprit <- bad_layer[bad_layer != min(bad_layer)]
  i <- which(xdf$name == names(culprit))
  l <- list(
    program = names(culprit),
    weight = xdf$weight[i],
    fixed_weight = min(bad_layer) - (xdf$total_weight[i] - xdf$weight[i])
  )
  message(
    "program ", l$program,
    ": given wt ", l$weight,
    ", fixed wt ", l$fixed_weight
  )
  invisible(l)
}

suggest_fix(example_input)
suggest_fix(jenny_input)
