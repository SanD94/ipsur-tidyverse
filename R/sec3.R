# Chapter 3 -- Data Descriptions
library(tidyverse)
library(timetk)

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


# ts object to tibble object to plot in ggplot2
LakeHuron %>%
  tk_tbl() %>%
  ggplot(mapping = aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") +
  ylab("LakeHuron")
LakeHuron %>%
  tk_tbl() %>%
  ggplot(mapping = aes(x = index, y = value)) +
  geom_point()
LakeHuron %>%
  tk_tbl() %>%
  ggplot(mapping = aes(x = index, y = value)) +
  geom_segment(aes(x = index, xend = index, y = min(value) - 1, yend = value)) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = expansion(0, c(1, 1)), oob = scales::oob_keep) +
  scale_y_continuous(expand = expansion(0, c(0, 1)), oob = scales::oob_keep)

faithful %>% ggplot(mapping = aes(x = eruptions)) +
  geom_density(bw = "sj")

# titanic
Titanic %>%
  as_tibble() %>%
  ggplot(mapping = aes(x = Class, y = n, fill = Survived)) +
  geom_bar(stat = "identity")

Titanic %>%
  as_tibble() %>%
  ggplot(mapping = aes(x = Class, y = n, fill = Survived)) +
  geom_bar(stat = "identity", position = "dodge")

Titanic %>%
  as_tibble() %>%
  ggplot(mapping = aes(x = Survived, y = n, fill = Class)) +
  geom_bar(stat = "identity")

# simulation of mosaic plot (x axis adjustment needed)
Titanic %>%
  as_tibble() %>%
  group_by(Survived) %>%
  mutate(prob = n / sum(n), w = sum(n)) %>%
  ungroup() %>%
  mutate(w = w / max(w)) %>%
  ungroup() %>%
  ggplot(mapping = aes(x = Survived, y = prob, fill = Class, width = w)) +
  geom_bar(stat = "identity") +
  theme_void()

# using ggmosaic
Titanic %>%
  as_tibble() %>%
  ggplot() +
  geom_mosaic(aes(x = product(Survived), weight = n, fill = Class), show.legend = FALSE) +
  theme_mosaic()
