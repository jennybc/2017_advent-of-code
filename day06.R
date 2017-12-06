hash <- function(x) paste(x, collapse = "-")

deja_vu <- function(x) {
  len <- length(x)
  x_hash <- character()
  n <- 0
  while(!hash(x) %in% x_hash) {
    n <- n + 1
    x_hash <- c(x_hash, hash(x))
    i_max <- which.max(x)
    redist <- integer(len * (max(x) %/% len + 2))
    redist[seq_len(x[i_max]) + i_max] <- 1
    x[i_max] <- 0
    x <- x + rowSums(matrix(redist, nrow = len))
  }
  c(n, n - which(x_hash == hash(x)) + 1)
}

deja_vu(c(0, 2, 7, 0))

x <- "2	8	8	5	4	2	3	1	5	5	1	2	15	13	5	14"
x <- as.integer(strsplit(x, "\\s+")[[1]])
deja_vu(x)
