library(testthat)

rev_captcha <- function(x) {
  x <- as.integer(strsplit(x, "")[[1]])
  xd <- diff(c(x, x[1]))
  sum(x[xd == 0])
}

expect_equal(rev_captcha("1122"), 3)
expect_equal(rev_captcha("1111"), 4)
expect_equal(rev_captcha("1234"), 0)
expect_equal(rev_captcha("91212129"), 9)

txt <- scan("R/day01_input.txt", what = "character")
rev_captcha(txt)

rev_captcha2 <- function(x, lag = length(x)/2) {
  x <- as.integer(strsplit(x, "")[[1]])
  xs <- c(tail(x, -lag), head(x, lag))
  sum(x[x == xs])
}

expect_equal(rev_captcha2("1212"), 6)
expect_equal(rev_captcha2("1221"), 0)
expect_equal(rev_captcha2("123425"), 4)
expect_equal(rev_captcha2("123123"), 12)
expect_equal(rev_captcha2("12131415"), 4)

txt <- scan("R/day01_input.txt", what = "character")
rev_captcha2(txt)

## revisit first examples
expect_equal(rev_captcha2("1122", 1), 3)
expect_equal(rev_captcha2("1111", 1), 4)
expect_equal(rev_captcha2("1234", 1), 0)
expect_equal(rev_captcha2("91212129", 1), 9)
