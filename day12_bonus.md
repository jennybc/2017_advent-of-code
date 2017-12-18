Advent of Code 2017, Day 12, bonus content
================
Jenny Bryan
2017-12-17

<http://adventofcode.com/2017/day/12> Playing with day 12, part 1, without using igraph.

``` r
prep <- function(s) {
  x <- gsub("^[0-9]+ <-> ", "", readLines(s))
  x <- strsplit(x, ", ")
  lapply(x, as.integer)
}

get_component <- function(g, v = 1) {
  disc <- rep(FALSE, length(g))
  dfs <- function(g, v = 1) {
    disc[v] <<- TRUE
    for (w in g[[v]]) {
      if (!disc[w]) {
        dfs(g, w)
      }
    }
  }
  dfs(g, v = v)
  which(disc)
}
```

Input from AoC example, but indexing from 1, not 0.

``` r
x <- c(
  "1 <-> 3",
  "2 <-> 2",
  "3 <-> 1, 4, 5",
  "4 <-> 3, 5",
  "5 <-> 3, 4, 7",
  "6 <-> 7",
  "7 <-> 5, 6"
)

g <- prep(textConnection(x))
(out <- get_component(g))
#> [1] 1 3 4 5 6 7
length(out)
#> [1] 6

## my R solution numbers everything starting from 1 (vs. 0 for AoC)
g <- prep("day12_input.txt")
g <- lapply(g, function(x) x + 1)
out <- get_component(g)
length(out)
#> [1] 141
```
