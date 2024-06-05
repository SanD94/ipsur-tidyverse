## Section 3 util functions
# central moment generating function
# special case that x prob same
moment_discrete <- function(x, d) {
  n <- length(x)
  m <- mean(x)
  1 / n * sum((x - m)^d)
}


# check e1071 module for details
# unbiased kurtosis in SPSS
# https://www.wikiwand.com/en/Kurtosis#Standard_unbiased_estimator
kurtosis <- function(x) {
  n <- length(x)
  m4 <- moment_discrete(x, 4)
  m2 <- moment_discrete(x, 2)

  a <- (n - 1) / ((n - 2) * (n - 3))
  b <- (n + 1) * m4 / m2^2
  c <- 3 * (n - 1)
  a * (b - c)
}

# skewness in SPSS
skewness <- function(x) {
  n <- length(x)
  a <- n / ((n - 1) * (n - 2))
  z <- scale(x)
  a * sum(z^3)
}


# Section 5
# x is the parameter
# prob function with a parameter x
moment <- function(x, f_x) {
  sum(x * f_x)
}

# moment generator function
generate_moment <- function(t, f_x) {
  moment(exp(t), f_x)
}
