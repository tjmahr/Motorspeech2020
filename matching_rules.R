
library(tidyverse)
library(ggplot2)
library(extrafont)
extrafont::loadfonts()

y_min <- 1
y_length <- .5
y_gap <- .25

set_y_start <- function(row) {
  row_count_above_first <- row - 1
  y_min +
    row_count_above_first * y_length +
    row_count_above_first * y_gap
}

set_y_end <- function(row) {
  set_y_start(row) + y_length
}



data_gold <- tibble(
  sound = "S",
  demo = c("Matching", "Not matching"),
  aligner = c("Manual", "Manual"),
  row = c(2, 2),
  start = c(1.3, 1.3),
  end = c(2.7, 2.7)
)

data_matching <- tibble(
  sound = "S",
  demo = c("Matching"),
  aligner = c("Automatic"),
  row = 1,
  start = c(.7),
  end = c(2.5)
)

data_nonmatching <- tibble(
  sound = "S",
  demo = c("Not matching"),
  aligner = c("Automatic"),
  row = 1,
  start = c(.7),
  end = c(1.8)
)



data <- bind_rows(data_gold, data_matching, data_nonmatching) %>%
  mutate(
    y = set_y_start(row),
    yend = set_y_end(row),
    ymid = (y + yend) / 2,
    xmid = (start + end) / 2,
  )

y_labels <- data %>%
  distinct(row, ymid, aligner) %>%
  arrange(row)

data_midpoints <- data %>%
  filter(aligner == "Manual") %>%
  group_by(demo) %>%
  summarise(
    y = ymid,
    x = xmid,
    yend = 1
  )

ggplot(data) +
  aes(y = aligner) +
  geom_segment(
    aes(x = start, xend = start, y = y, yend = yend),
    size = 2
  ) +
  geom_segment(
    aes(x = end, xend = end, y = y, yend = yend),
    size = 2
  ) +
  geom_segment(
    aes(y = y, yend = yend, x = x, xend = x),
    data = data_midpoints,
    linetype = "dotted",
    size = 1.25
  ) +
  geom_label(
    aes(x = xmid, y = ymid, label = sound),
    size = 12,
    label.size = 0,
    fill = "white",
    label.padding = unit(1, "lines"),
    family = "Lato Medium"
  ) +
  # theme_minimal() +
  scale_y_continuous(
    breaks = y_labels$ymid,
    labels = y_labels$aligner,
    minor_breaks = NULL
  ) +
  facet_wrap("demo") +
  theme_minimal(
    base_size = 36, base_family = "Lato Medium"
  ) +
  theme(
    axis.text.y = element_text(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = "grey40"),
    axis.text.x = element_blank()
  ) +
  labs(x = NULL, y = NULL) +
  xlim(0, 3)

tjmisc::ggpreview(width = 12, height = 4, dpi = 600)
