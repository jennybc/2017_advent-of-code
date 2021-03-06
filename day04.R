#' ---
#' title: "Day 4"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---

#+ setup, include = FALSE, cache = FALSE
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  error = TRUE
)
options(tidyverse.quiet = TRUE)

#' <http://adventofcode.com/2017/day/4>

library(testthat)
library(tidyverse)

#is_valid <- function(x) length(x) == n_distinct(x)
is_valid <- function(x) !anyDuplicated(x)

normalize <- function(x) {
  xs <- strsplit(x, "")
  xs %>% map(sort) %>% map(paste0, collapse = "") %>% flatten_chr()
}

check_validity <- . %>%
  mutate(
    pph = strsplit(pph, "\\s"),
    pph_norm = map(pph, normalize),
    valid = map_lgl(pph, is_valid),
    valid_norm = map_lgl(pph_norm, is_valid)
  )

test1 <- tribble(
               ~pph,  ~ref,
   "aa bb cc dd ee",  TRUE,
   "aa bb cc dd aa", FALSE,
  "aa bb cc dd aaa",  TRUE
) %>% check_validity()
expect_identical(test1$ref, test1$valid)

test2 <- tribble(
                       ~ pph, ~ ref,
               "abcde fghij",  TRUE,
           "abcde xyz ecdab", FALSE,
      "a ab abc abd abf abj",  TRUE,
  "iiii oiii ooii oooi oooo",  TRUE,
       "oiii ioii iioi iiio", FALSE
) %>% check_validity()
expect_identical(test2$ref, test2$valid_norm)

## The Real Deal
df <- tibble(pph = readLines("day04_input.txt")) %>% check_validity()
sum(df$valid)

sum(df$valid_norm)
