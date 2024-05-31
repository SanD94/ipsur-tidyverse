# Chapter 3 -- Data Descriptions
library(tidyverse)
library(zoo)

# ggplot equivalance of different plot with default libraries
ggplot(mapping = aes(x = precip, y = 1)) +
  geom_point() +
  xlab("rainfall")
ggplot(mapping = aes(x = rivers, y = 1)) +
  geom_point(position = "jitter") +
  xlab("length")
ggplot(mapping = aes(x = discoveries %>% as.integer())) +
  geom_bar() +
  xlab("number")

ggplot() +
  geom_histogram(aes(precip), bins = 7)
ggplot() +
  geom_histogram(aes(precip, after_stat(density)), bins = 7)

# different bins
ggplot() +
  geom_histogram(aes(precip), bins = 10)
ggplot() +
  geom_histogram(aes(precip), bins = 25)
ggplot() +
  geom_histogram(aes(precip), bins = 50)


# ts object to zoo object to plot with ggplot2
LakeHuron %>%
  fortify.zoo(melt = TRUE) %>%
  ggplot(mapping = aes(x = Index, y = Value)) +
  geom_line() +
  xlab("Time") +
  ylab("LakeHuron")
LakeHuron %>%
  fortify.zoo(melt = TRUE) %>%
  ggplot(mapping = aes(x = Index, y = Value)) +
  geom_point()
LakeHuron %>%
  fortify.zoo(melt = TRUE) %>%
  ggplot(mapping = aes(x = Index, y = Value)) +
  geom_segment(aes(x = Index, xend = Index, y = min(Value) - 1, yend = Value)) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = expansion(0, c(1, 1)), oob = scales::oob_keep) +
  scale_y_continuous(expand = expansion(0, c(0, 1)), oob = scales::oob_keep)

faithful %>% ggplot(mapping = aes(x = eruptions)) +
  geom_density(bw = "sj")
