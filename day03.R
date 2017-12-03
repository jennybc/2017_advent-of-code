library(testthat)
library(tidyverse)

## part 1

## how many squares are along a side of current layer?
##       1 -> 1
##   2 - 9 -> 3
## 10 - 25 -> 5
m <- function(x) {
  ret <- ceiling(sqrt(x))
  ret + (ret %% 2 == 0)
}

manhattan <- function(x) {
  xm <- m(x)
  r <- (xm - 1)/2
  lower_right <- xm ^ 2
  midpoints <- lower_right - r * c(1, 3, 5, 7)
  min(abs(x - midpoints)) + r
}

expect_equal(manhattan(1), 0)
expect_equal(manhattan(2), 1)
expect_equal(manhattan(12), 3)
expect_equal(manhattan(23), 2)
expect_equal(manhattan(1024), 31)

## my input
manhattan(347991)
## 480



## part 2

## I'm going to brute force this

df <- tibble(
  k = 0:4,
  len = k * 2 + 1,                 ## how many squares on a side = 1, 3, 5, ...
  i_start = k,                     ## horizontal position of first square
  j_start = -1 * c(0, head(k, -1)) ## vertical position of first square
)

## len squares on a side
## t = len - 1 (don't double-visit a square)
## returns tibble of i and j offsets, relative to first square, of all
## squares in this layer
rel_pos <- function(t) {
  if (t == 0) return(tibble(i_offset = 0, j_offset = 0))
  vec <- c(rep(0, t), 1:t, rep(t, t), t:1 - 1)
  tibble(
    i_offset = -1 * vec,
    j_offset = c(tail(vec, -t), head(vec, t)) - 1
  )
}

## expand from one row per layer to one row per square
bf <- df %>%
  mutate(data = map(len - 1, rel_pos)) %>%
  unnest() %>%
  mutate(
    i = i_start + i_offset,
    j = j_start + j_offset,
    w = row_number(),
    val = NA_real_
  )

## "store the value 1 in square 1
## in the same allocation order as shown above,
## store the sum of the values in all adjacent squares"
bf$val[1] <- 1
for(h in 2:nrow(bf)) {
  this_i <- bf$i[h]
  this_j <- bf$j[h]
  ok <- between(bf$i, this_i - 1, this_i + 1) &
    between(bf$j, this_j - 1, this_j + 1) &
    bf$w < h
  bf$val[h] <- sum(bf$val[ok])
}

ref <- c(1, 1, 2, 4, 5, 10, 11, 23, 25, 26, 54, 57, 59, 122, 133, 142, 147,
         304, 330, 351, 362, 747, 806)
expect_equal(ref, bf$val[seq_along(ref)])
## yassss

bf %>%
  filter(val > 347991) %>%
  slice(1) %>%
  pull(val)
## 349975
