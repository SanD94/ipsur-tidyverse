library(httpgd)
library(showtext)


hgd() # check figures from the website served
showtext_auto()
font_add_google("Lato", "lato")


x <- mtcars$am

compute_loss <- function(p, x) prod(dbinom(x, size = 1, prob = p))
optimize(compute_loss, interval = c(0, 1), x = x, maximum = TRUE)

minus_log_loss <- function(p, x) {
  -sum(dbinom(x, size = 1, prob = p, log = TRUE))
}
optimize(minus_log_loss, interval = c(0, 1), x = x)

minus_log_gauss <- function(mu, sigma2) {
  -sum(dnorm(x, mean = mu, sd = sqrt(sigma2), log = TRUE)) 
}


library(stats4)
x <- PlantGrowth$weight
max_like_est <- mle(minus_log_gauss, start = list(mu = 5, sigma2 = 0.5))
summary(max_like_est)

mean(x)
var(x) * 29 / 30
sd(x) / sqrt(30)


library(RcmdrMisc)

x <- with(morley, Speed[Expt == 1])
qqPlot(x)


library(TeachingDemos)
with(PlantGrowth, z.test(weight, sd = 0.7))


x <- with(morley, Speed[Expt == 1])
t.test(x, conf.level = 0.99, alternative = "greater")


# Between two-sample t-test
x <- with(ToothGrowth, len[supp == "VC"])
y <- with(ToothGrowth, len[supp == "OJ"])

t.test(x, y, conf.level = 0.99)
t.test(x, y, conf.level = 0.99, var.equal = TRUE)
# TODO: check ggpubr qqplot function
qqPlot(x)
qqPlot(y)

t.test(len ~ supp, data = ToothGrowth, conf.level = 0.99)


# Confidence Intervals for Proportions
library(Hmisc)

### Wald interval -- it applies CLT as approximation
binconf(x = 7, n = 25, alpha = 0.05, method = "asymptotic")

### Wilson interval
binconf(x = 7, n = 25, alpha = 0.05, method = "wilson")

### default method is wilson
binconf(x = 7, n = 25)

### prop.test supports one sided interval estimation
prop.test(x = 7, n = 25, conf.level = 0.95, correct = FALSE)
prop.test(x = 7, n = 25, alternative = "greater", correct = FALSE)


### two sample p1 - p2 == 0
prop.test(x = c(7, 11), n = c(25, 37), conf.level = 0.99, correct = FALSE)


# Confidence Intervals for Variances
x <- with(morley, Speed[Expt == 1])
library(TeachingDemos)
sigma.test(x, conf.level = 0.99)

var.test(len ~ supp, data = ToothGrowth, conf.level = 0.99)


# Sample Size and Margin of Error
power.t.test(delta = 1, sd = 105, sig.level = 0.05,
             type = "one.sample", power = 0.5)

power.t.test(n = 20, delta = 1)
