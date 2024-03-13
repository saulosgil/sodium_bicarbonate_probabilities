# Libraries
library(tidyverse)

# Load and preprocessing ----------------------------------------------------------------------
data <- read_delim(
  "data_bubleplot_study2.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)

# Create condition color
data <-
  data |>
  mutate(Condition = factor(
    Condition,
    levels = c(
      "Gelatine 0.3",
      "Solution 0.3",
      "Enteric 0.3",
      "Enteric 0.1"
    )
  )) |>
  filter(!is.na(Condition))

glimpse(data)

# Basic Bubble plot
# data |>
#   ggplot(aes(
#     x = Time,
#     y = score_nausea,
#     size = size_nausea,
#   )) +
#   geom_point(alpha = 0.3,
#              shape = 21,
#              fill = "black",
#              color = "black") +
#   scale_size(range = c(2, 8),
#              name = "Positive for symptoms") +
#   scale_y_continuous(
#     limits = c(-2, 12),
#     breaks = c(0, 2, 4, 6, 8, 10),
#     labels = c("0", "2", "4", "6", "8", "10")
#   ) +
#   ylab("Score") +
#   xlab("Time") +
#   theme_bw() +
#   theme(
#     legend.position = "top"
#   ) +
#   facet_grid(Condition ~ .)

# all vars plots ------------------------------------------------------------------------------
# Nausea
nausea <-
  data |>
  ggplot(aes(x = Time,
             y = score_nausea,
             size = size_nausea)) +
  geom_point(
    alpha = 0.3,
    shape = 21,
    fill = "black",
    color = "black"
  ) +
  scale_size(range = c(2, 12),
             name = "Positive for symptoms") +
  scale_y_continuous(
    limits = c(-2, 12),
    breaks = c(0, 2, 4, 6, 8, 10),
    labels = c("0", "2", "4", "6", "8", "10")
  ) +
  ylab("Score") +
  xlab("Time") +
  labs(title = "Nausea") +
  theme_bw() +
  theme(legend.position = "top") +
  facet_grid(Condition ~ .)

nausea

# Gastric Distress
gastric_distress <-
  data |>
  ggplot(aes(x = Time,
             y = score_gatric_distress,
             size = size_gatric_distress)) +
  geom_point(
    alpha = 0.3,
    shape = 21,
    fill = "black",
    color = "black"
  ) +
  scale_size(range = c(2, 10),
             name = "Positive for symptoms") +
  scale_y_continuous(
    limits = c(-2, 12),
    breaks = c(0, 2, 4, 6, 8, 10),
    labels = c("0", "2", "4", "6", "8", "10")
  ) +
  ylab("Score") +
  xlab("Time") +
  labs(title = "Gastric Distress") +
  theme_bw() +
  theme(legend.position = "top") +
  facet_grid(Condition ~ .)

gastric_distress

# belching
bealching <-
  data |>
  ggplot(aes(x = Time,
             y = score_belching,
             size = size_belching)) +
  geom_point(
    alpha = 0.3,
    shape = 21,
    fill = "black",
    color = "black"
  ) +
  scale_size(range = c(2, 12),
             name = "Positive for symptoms") +
  scale_y_continuous(
    limits = c(-2, 12),
    breaks = c(0, 2, 4, 6, 8, 10),
    labels = c("0", "2", "4", "6", "8", "10")
  ) +
  ylab("Score") +
  xlab("Time") +
  labs(title = "Belching")+
  theme_bw() +
  theme(legend.position = "top") +
  facet_grid(Condition ~ .)

bealching

# intestinal_bloating
intestinal_bloating <-
  data |>
  ggplot(aes(x = Time,
             y = score_intestinal_bloating,
             size = size_intestinal_bloating)) +
  geom_point(
    alpha = 0.3,
    shape = 21,
    fill = "black",
    color = "black"
  ) +
  scale_size(range = c(2, 8),
             name = "Positive for symptoms") +
  scale_y_continuous(
    limits = c(-2, 12),
    breaks = c(0, 2, 4, 6, 8, 10),
    labels = c("0", "2", "4", "6", "8", "10")
  ) +
  ylab("Score") +
  xlab("Time") +
  labs(title = "Intestinal Bloating")+
  theme_bw() +
  theme(legend.position = "top") +
  facet_grid(Condition ~ .)

intestinal_bloating

# Bloating
bloating <-
  data |>
  ggplot(aes(x = Time,
             y = score_bloating,
             size = size_bloating)) +
  geom_point(
    alpha = 0.3,
    shape = 21,
    fill = "black",
    color = "black"
  ) +
  scale_size(range = c(2, 8),
             name = "Positive for symptoms") +
  scale_y_continuous(
    limits = c(-2, 12),
    breaks = c(0, 2, 4, 6, 8, 10),
    labels = c("0", "2", "4", "6", "8", "10")
  ) +
  ylab("Score") +
  xlab("Time") +
  labs(title = "Bloating") +
  theme_bw() +
  theme(legend.position = "top") +
  facet_grid(Condition ~ .)

bloating

# Flatulence
flatulence <-
  data |>
  ggplot(aes(x = Time,
             y = score_flatulence,
             size = size_flatulence)) +
  geom_point(
    alpha = 0.3,
    shape = 21,
    fill = "black",
    color = "black"
  ) +
  scale_size(range = c(6, 12),
             name = "Positive for symptoms") +
  scale_y_continuous(
    limits = c(-2, 12),
    breaks = c(0, 2, 4, 6, 8, 10),
    labels = c("0", "2", "4", "6", "8", "10")
  ) +
  ylab("Score") +
  xlab("Time") +
  labs(title = "Flatulence") +
  theme_bw() +
  theme(legend.position = "top") +
  facet_grid(Condition ~ .)

flatulence

# Intestine upset
intestine_upset <-
  data |>
  ggplot(aes(x = Time,
             y = score_intestine_upset,
             size = size_intestine_upset)) +
  geom_point(
    alpha = 0.3,
    shape = 21,
    fill = "black",
    color = "black"
  ) +
  scale_size(range = c(2, 8),
             name = "Positive for symptoms") +
  scale_y_continuous(
    limits = c(-2, 12),
    breaks = c(0, 2, 4, 6, 8, 10),
    labels = c("0", "2", "4", "6", "8", "10")
  ) +
  ylab("Score") +
  xlab("Time") +
  labs(title = "Intestine upset") +
  theme_bw() +
  theme(legend.position = "top") +
  facet_grid(Condition ~ .)

intestine_upset

# Headache
headache <-
  data |>
  ggplot(aes(x = Time,
             y = score_headache,
             size = size_headache)) +
  geom_point(
    alpha = 0.3,
    shape = 21,
    fill = "black",
    color = "black"
  ) +
  scale_size(range = c(2, 10),
             name = "Positive for symptoms") +
  scale_y_continuous(
    limits = c(-2, 12),
    breaks = c(0, 2, 4, 6, 8, 10),
    labels = c("0", "2", "4", "6", "8", "10")
  ) +
  ylab("Score") +
  xlab("Time") +
  labs(title = "Headache") +
  theme_bw() +
  theme(legend.position = "top") +
  facet_grid(Condition ~ .)

headache



