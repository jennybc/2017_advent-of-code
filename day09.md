Advent of Code 2017, Day 9
================
Jenny Bryan
2017-12-10

<http://adventofcode.com/2017/day/9>

``` r
library(tidyverse)
library(testthat)
#> 
#> Attaching package: 'testthat'
#> The following object is masked from 'package:dplyr':
#> 
#>     matches
#> The following object is masked from 'package:purrr':
#> 
#>     is_null
```

### Functions

I kept detailed data on each group even in part 1, betting that I would need it in part 2. That also made me shy away from leaning on regular expressions for pre-processing. In hindsight, I was being paranoid and this could be much simpler :( Oh well, win some, lose some.

``` r
jparse <- function(s, i = 1, k = 0, l = list()) {
  while(i <= length(s)) {
    if (s[i] == "!") {
      i <- i + 2
      next
    }

    if (k < 0) {         ## we've been in garbage
      if (s[i] == ">") { ## garbage ends HERE
        k <- k * -1
      } else {           ## still in garbage
        l[[length(l)]][["g"]] <- l[[length(l)]][["g"]] + 1
      }
      i <- i + 1
      next
    }

    if (s[i] == "{") {
      k <- k + 1
      ## stop = placeholder for where group ends (but I never needed it)
      ## g = number of garbage characters in this group
      l[[length(l) + 1]] <- list(start = i, stop = NA, level = k, g = 0)
    }

    if (s[i] == "}") {
      k <- k - 1
      ## populate stop of the last component of l here, if we ever need it
    }

    if(s[i] == "<") {
      k <- k * -1
    }

    i <- i + 1
  }
  return(l)
}

parse_string <- function(s) {
  s <- strsplit(s, "")[[1]]
  jparse(s)
}
```

### Working with my input

``` r
out <- parse_string(scan("day09_input.txt", what = ""))
out %>% map_dbl("level") %>% sum()
#> [1] 11898
out %>% map_dbl("g") %>% sum()
#> [1] 5601
```

### Testing

I did these tests before using my own input.

``` r
garbage_examples <- tribble(
               ~ string,            ~ result, ~ nchar,         ~ notes,
                   "<>",                  "",       0,         "empty garbage",
  "<random characters>", "random characters",      17,                      "",
                "<<<<>",               "<<<",       3,   "extra < are ignored",
               "<{!>}>",              "{!>}",       2,     "1st > is canceled",
                 "<!!>",                "!!",       0,     "2nd ! is canceled",
               "<!!!>>",              "!!!>",       0, "2nd !, 1st > canceled",
      "<{o\"i!a,<{i<a>",     "{o\"i!a,<{i<a",      10,         "ends at 1st >"
)

group_examples <- tribble(
                     ~ string, ~ n_groups,
                         "{}",          1,
                     "{{{}}}",          3,
                    "{{},{}}",          3,
             "{{{},{},{{}}}}",          6,
             "{<{},{},{{}}>}",          1,
          "{<a>,<a>,<a>,<a>}",          1,
  "{{<a>},{<a>},{<a>},{<a>}}",          5,
  "{{<!>},{<!>},{<!>},{<a>}}",          2
)

score_examples <- tribble(
                         ~ string, ~ score,                ~ explanation,
                             "{}",       1,                      "1 = 1",
                         "{{{}}}",       6,              "1 + 2 + 3 = 6",
                        "{{},{}}",       5,              "1 + 2 + 2 = 5",
                 "{{{},{},{{}}}}",      16, "1 + 2 + 3 + 3 + 3 + 4 = 16",
              "{<a>,<a>,<a>,<a>}",       1,                      "1 = 1",
  "{{<ab>},{<ab>},{<ab>},{<ab>}}",       9,      "1 + 2 + 2 + 2 + 2 = 9",
  "{{<!!>},{<!!>},{<!!>},{<!!>}}",       9,      "1 + 2 + 2 + 2 + 2 = 9",
  "{{<a!>},{<a!>},{<a!>},{<ab>}}",       3,                  "1 + 2 = 3"
)

out <- map(group_examples$string, parse_string)
(df <- bind_cols(group_examples, res = lengths(out)))
#> # A tibble: 8 x 3
#>                      string n_groups   res
#>                       <chr>    <dbl> <int>
#> 1                        {}        1     1
#> 2                    {{{}}}        3     3
#> 3                   {{},{}}        3     3
#> 4            {{{},{},{{}}}}        6     6
#> 5            {<{},{},{{}}>}        1     1
#> 6         {<a>,<a>,<a>,<a>}        1     1
#> 7 {{<a>},{<a>},{<a>},{<a>}}        5     5
#> 8 {{<!>},{<!>},{<!>},{<a>}}        2     2
expect_equal(df$n_groups, df$res)

out <- map(score_examples$string, parse_string)
scores <- out %>%
  map(. %>% map_dbl("level")) %>%
  map_dbl(sum)
(df <- bind_cols(
  select(score_examples, -explanation),
  res = scores
))
#> # A tibble: 8 x 3
#>                          string score   res
#>                           <chr> <dbl> <dbl>
#> 1                            {}     1     1
#> 2                        {{{}}}     6     6
#> 3                       {{},{}}     5     5
#> 4                {{{},{},{{}}}}    16    16
#> 5             {<a>,<a>,<a>,<a>}     1     1
#> 6 {{<ab>},{<ab>},{<ab>},{<ab>}}     9     9
#> 7 {{<!!>},{<!!>},{<!!>},{<!!>}}     9     9
#> 8 {{<a!>},{<a!>},{<a!>},{<ab>}}     3     3
expect_equal(df$score, df$res)

out <- map(paste0("{", garbage_examples$string, "}"), parse_string)
(df <- bind_cols(
  select(garbage_examples, -notes),
  res = out %>% map_dbl(. %>% map_dbl("g"))
))
#> # A tibble: 7 x 4
#>                string            result nchar   res
#>                 <chr>             <chr> <dbl> <dbl>
#> 1                  <>                       0     0
#> 2 <random characters> random characters    17    17
#> 3               <<<<>               <<<     3     3
#> 4              <{!>}>              {!>}     2     2
#> 5                <!!>                !!     0     0
#> 6              <!!!>>              !!!>     0     0
#> 7   "<{o\"i!a,<{i<a>"   "{o\"i!a,<{i<a"    10    10
expect_equal(df$nchar, df$res)
```
