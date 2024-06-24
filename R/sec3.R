# Chapter 3 -- Data Descriptions
library(tidyverse)
library(patchwork)
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
# TODO: width and height adjustment, big space between bars
Titanic %>%
  as_tibble() %>%
  group_by(Survived) %>%
  mutate(prob = n / sum(n), w = sum(n)) %>%
  ungroup() %>%
  mutate(w = w / sum(n)) %>%
  ungroup() %>%
  ggplot(mapping = aes(x = Survived, y = prob, fill = Class, width = w)) +
  geom_bar(stat = "identity") +
  theme_void()

puro_f <- Puromycin %>%
  ggplot(mapping = aes(x = conc, y = rate)) +
  geom_point()

atten_f <- attenu %>%
  ggplot(mapping = aes(x = dist, y = accel)) +
  geom_point()

puro_f + atten_f


# simmilar to spine plot without x axis adjustment
tibble(
  region = state.region,
  division = state.division
) %>% ggplot(aes(x = region)) +
  geom_bar(aes(fill = division), position = "fill")
