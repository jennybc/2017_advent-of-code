#' ---
#' title: "Advent of Code 2017, Day 9"
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

#' <http://adventofcode.com/2017/day/9>

library(tidyverse)
library(testthat)

## https://stackoverflow.com/questions/14952113/how-can-i-match-nested-brackets-using-regex

garbage_examples <- tribble(
  ~ string, ~ result, ~ notes,
  "<>", "", "empty garbage",
  "<random characters>", "random characters", "",
  "<<<<>", "<<<", "because the extra < are ignored",
  "<{!>}>", "{!>}", "because the first > is canceled",
  "<!!>", "!!", "because the second ! is canceled, allowing the > to terminate the garbage",
  "<!!!>>", "!!!>", "because the second ! and the first > are canceled",
  "<{o\"i!a,<{i<a>", "{o\"i!a,<{i<a", "which ends at the first >"
)

group_examples <- tribble(
  ~ string, ~n_groups,
  "{}", 1,
  "{{{}}}", 3,
  "{{},{}}", 3,
  "{{{},{},{{}}}}", 6,
  "{<{},{},{{}}>}", 1,
  "{<a>,<a>,<a>,<a>}", 1,
  "{{<a>},{<a>},{<a>},{<a>}}", 5,
  "{{<!>},{<!>},{<!>},{<a>}}", 2
)

score_examples <- tribble(
  ~ string, ~ score, ~ explanation,
  "{}", 1, "1 = 1",
  "{{{}}}", 6, "1 + 2 + 3 = 6",
  "{{},{}}", 5, "1 + 2 + 2 = 5",
  "{{{},{},{{}}}}", 16, "1 + 2 + 3 + 3 + 3 + 4 = 16",
  "{<a>,<a>,<a>,<a>}", 1, "1 = 1",
  "{{<ab>},{<ab>},{<ab>},{<ab>}}", 9, "1 + 2 + 2 + 2 + 2 = 9",
  "{{<!!>},{<!!>},{<!!>},{<!!>}}", 9, "1 + 2 + 2 + 2 + 2 = 9",
  "{{<a!>},{<a!>},{<a!>},{<ab>}}", 3, "1 + 2 = 3"
)


