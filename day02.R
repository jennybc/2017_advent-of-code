library(purrr)
library(magrittr)
library(readr)
library(testthat)

prep <- function(txt) {
  txt %>%
    read_lines() %>%
    strsplit("\\s") %>%
    map(as.integer)
}

span <- function(x) diff(range(x))
dvr <- function(x) {
  o <- outer(x, x, `%%`)
  i <- which(rowSums(o == 0) > 1)
  j <- which(colSums(o == 0) > 1)
  x[i]/x[j]
}

checksum <- function(xl, f) sum(map_dbl(xl, f))

x <- prep("5 1 9 5\n7 5 3\n2 4 6 8")
expect_equal(checksum(x, span), 18)

x <- prep("day02_input.txt")
checksum(x, span)

x <- prep("5 9 2 8\n9 4 7 3\n3 8 6 5")
expect_equal(checksum(x, dvr), 9)

x <- prep("day02_input.txt")
checksum(x, dvr)
