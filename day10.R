#' ---
#' title: "Advent of Code 2017, Day 10"
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

#' <http://adventofcode.com/2017/day/10>

library(tidyverse)
library(testthat)

jmod <- function(x, n = 5) (x - 1) %% n + 1

jprod <- function(n, l) {
  x <- seq_len(n) - 1
  cpos <- 1
  i <- 0
  while(i < length(l)) {
    jseq <- jmod(seq(from = cpos, length.out = l[i + 1]), n)
    x[jseq] <- rev(x[jseq])
    cpos <- jmod(cpos + (l[i + 1] + i), n)
    i <- i + 1
  }
  prod(x[1:2])
}

expect_equal(jprod(n = 5, l = c(3, 4, 1, 5)), 12)

jprod(n = 256, l = c(157,222,1,2,177,254,0,228,159,140,249,187,255,51,76,30))

## part 2

## convert to ascii codes, append standard suffix
prep <- function(z) c(utf8ToInt(z), 17, 31, 73, 47, 23)

expect_equal(
  prep("1,2,3"),
  c(49,44,50,44,51,17,31,73,47,23)
)

sparse_hash <- function(l, n = 256) {
  x <- seq_len(n) - 1
  cpos <- 1
  sks <- 0
  for (j in seq_len(64)) {
    i <- 1
    while(i <= length(l)) {
      jseq <- jmod(seq(from = cpos, length.out = l[i]), n)
      x[jseq] <- rev(x[jseq])
      cpos <- jmod(cpos + (l[i] + sks), n)
      i <- i + 1
      sks <- sks + 1
    }
  }
  x
}

knot_hash <- function(l, n = 256) {
  sps_h <- sparse_hash(l = l)
  sps_hmat <- matrix(sps_h, nrow = sqrt(n))
  dense_hash <- apply(sps_hmat, 2, function(x) Reduce(bitwXor, x))
  paste(as.hexmode(dense_hash), collapse = "")
}

expect_equal(
  knot_hash(prep("")),
  "a2582a3a0e66e6e86e3812dcb672a272"
)
expect_equal(
  knot_hash(prep("AoC 2017")),
  "33efeb34ea91902bb2f59c9920caa6cd"
)
expect_equal(
  knot_hash(prep("1,2,3")),
  "3efbe78a8d82f29979031a4aa0b16a9d"
)
expect_equal(
  knot_hash(prep("1,2,4")),
  "63960835bcdc130f0b66d7ff4f6a5a8e"
)

## my input
knot_hash(prep("157,222,1,2,177,254,0,228,159,140,249,187,255,51,76,30"))
