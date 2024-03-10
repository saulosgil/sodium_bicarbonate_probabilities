# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)

# Load and preprocessing ----------------------------------------------------------------------
data <- read_delim(
  "data_bubleplot.csv",
  delim = ";",
  escape_double = FALSE,
  col_types = cols(
    Time = col_integer(),
    size_nausea = col_number(),
    size_gatric_distress = col_number(),
    size_belching = col_number(),
    size_heartnurn = col_number(),
    size_bloating = col_number(),
    size_flatulence = col_number(),
    size_urge_defecate = col_number(),
    size_intestine_upset = col_number(),
    size_dizziness = col_number(),
    size_headache = col_number()
  ),
  trim_ws = TRUE
)

# Create condition color
data <-
  data |>
  mutate(
    condition_color  = case_when(
      Condition == 'Gelatine 0.3' ~ 0,
      Condition == 'Gelatine 0.1' ~ 1,
      Condition == 'Resistant 0.3' ~ 2,
      Condition == 'Resistant 0.1' ~ 3,
      Condition == 'Placebo' ~ 4
    )
  )

glimpse(data)

# Basic Bubble plot
data |>
  ggplot(aes(
    x = Time,
    y = score_nausea,
    size = size_nausea,
    fill = Condition
  )) +
  geom_point(alpha = 0.3,
             shape = 21,
             color = "black") +
  scale_size(range = c(8, 16),
             name = "Positive for symptoms") +
  scale_fill_grey() +
  ylab("Score") +
  xlab("Time") +
  theme_bw() +
  theme(legend.position = "right") +
  facet_grid(Condition ~ .)

# Function ------------------------------------------------------------------------------------
# function to repeat

plot_fct <-
  function(base, score, size) {
    base |>
      ggplot(aes(
        x = Time,
        y = score,
        size = size,
        fill = Condition
      )) +
      geom_point(alpha = 0.3,
                 shape = 21,
                 color = "black") +
      scale_size(range = c(8, 16),
                 name = "Positive for symptoms") +
      scale_fill_grey() +
      xlab("Time") +
      theme_bw() +
      theme(legend.position = "right") +
      facet_grid(Condition ~ .)
  }

# all vars plots ------------------------------------------------------------------------------
# Nausea
nausea <-
  plot_fct(data,
         score = data$score_nausea,
         size = data$size_nausea) +
  labs(title = 'Nausea')

# Gastric Distress
gastric_distress <-
  plot_fct(data,
         score = data$score_gatric_distress,
         size = data$size_gatric_distress) +
  labs(title = 'Gastric Distress')

# Belching
bealching <-
  plot_fct(data,
         score = data$score_belching,
         size = data$size_belching) +
  labs(title = 'Belching')

# Heartburn
heartburn <-
  plot_fct(data,
         score = data$score_heartnurn,
         size = data$size_heartnurn) +
  labs(title = 'Heartburn')

# Bloating
bloating <-
  plot_fct(data,
         score = data$score_bloating,
         size = data$size_bloating) +
  labs(title = 'Bloating')

# Flatulence
flatulence <-
  plot_fct(data,
         score = data$score_flatulence,
         size = data$size_flatulence) +
  labs(title = 'Flatulence')

# Urge to defecate
urge_defecate <-
  plot_fct(data,
         score = data$score_urge_defecate,
         size = data$size_urge_defecate) +
  labs(title = 'Urge to defecate')

# Intestine upset
intestine_upset <-
  plot_fct(data,
         score = data$score_intestine_upset,
         size = data$size_intestine_upset) +
  labs(title = 'Intestine upset')

# Dizziness
dizziness <-
  plot_fct(data,
         score = data$score_dizziness,
         size = data$size_dizziness) +
  labs(title = 'Dizziness')

# Headache
headache <-
  plot_fct(data,
         score = data$score_headache,
         size = data$size_headache) +
  labs(title = 'Headache')




