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
