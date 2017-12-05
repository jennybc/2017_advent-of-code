library(testthat)

n_escape <- function(x, inc = function(k) k + 1) {
  inside <- function(i) i > 0 && i <= length(x)
  i <- 1
  n <- 0
  while(inside(i)) {
    jump <- x[i]
    x[i] <- inc(x[i])
    n <- n + 1
    i <- i + jump
  }
  n
}

expect_equal(n_escape(c(0, 3, 0, 1, -3)), 5)
expect_equal(
  n_escape(c(0, 3, 0, 1, -3), inc = function(k) k + if (k < 3) 1 else -1),
  10
)

x <- scan("day05_input.txt", what = integer())
n_escape(x)
# 355965
n_escape(x, inc = function(k) k + if (k < 3) 1 else -1)
# 26948068
