library(TeachingDemos)
library(IPSUR)

t <- prop.test(1755, n = 4526, p = 0.4, alternative = "less", correct = FALSE)
plot(t)

addmargins(xtabs(Freq ~ Sex + Survived, data = Titanic), margin=2)
prop.test(x = c(1364, 126), n = c(1731, 470), correct = FALSE)
