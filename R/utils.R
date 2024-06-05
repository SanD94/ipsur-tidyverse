## Section 3 util functions

# check e1071 module for details
# unbiased kurtosis in SPSS
# https://www.wikiwand.com/en/Kurtosis#Standard_unbiased_estimator
kurtosis <- function(x) {
  n <- length(x)
  m4 <- moment(x, g = function(x) x^4, central = TRUE)
  m2 <- moment(x, g = function(x) x^2, central = TRUE)

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
# x is the parameter 1d vector
# prob function with a parameter x
# default f_x uniform
# default g identity
moment <- function(x,
                   f_x = function(x) 1 / length(x),
                   g = function(x) x,
                   central = FALSE) {
  if (central) x <- x - mean(x)
  sum(g(x) * f_x(x))
}


# moment generator function
generate_moment <- function(x, f_x = function(x) 1 / length(x), t = 1) {
  g <- function(x) exp(x * t)
  moment(x, f_x, g)
}

# derivative of a moment generator at t = 0, d > 0
d_gen_moment <- function(x, f_x = function(x) 1 / length(x), d = 1) {
  g <- function(x) x^d
  moment(x, f_x, g)
}
