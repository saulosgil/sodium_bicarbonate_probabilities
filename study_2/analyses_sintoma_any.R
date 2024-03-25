# library -------------------------------------------------------------------------------------
library(readxl)
library(prob)
library(dplyr)

# reading dataset -----------------------------------------------------------------------------
df <- read_excel("study_2/conditional_prob_study2.xlsx",
                 sheet = "sintoma_any")

# to show severe symptoms (>1) ----------------------------------------------------------------
df_selected <-
  df |>
  select(id,
         teve_ao_menos_um_sint_any,
         mais_1_sint_any)

df_selected |>
  count(teve_ao_menos_um_sint_any,
        mais_1_sint_any,
        sort = TRUE)

df_selected

# Creating two-way table from data frame
df_selected_Table <-
  addmargins(
    table(
      "teve_ao_menos_um_sint_any" = df_selected$teve_ao_menos_um_sint_any,
      "mais_1_sint_any" = df_selected$mais_1_sint_any
    )
  )

# view table
df_selected_Table

df_selected <- probspace(df_selected)

df_selected

# Probability of the individual to show more than 1 severe symptoms given that they showed at least
# severe symptom

# using prob function of the prob pkg
prob(df_selected,
     event = mais_1_sint_any == "teve",
     given = teve_ao_menos_um_sint_any == "teve_sintoma_any")

# resposta = 0.2857143 (28%)

# to show more than 2 severe symptoms ----------------------------------------------------------
df_selected <-
  df |>
  select(id,
         teve_ao_menos_um_sint_any,
         mais_2_sint_any)

df_selected |>
  count(teve_ao_menos_um_sint_any,
        mais_2_sint_any,
        sort = TRUE)

df_selected

# Creating two-way table from data frame
df_selected_Table <-
  addmargins(
    table(
      "teve_ao_menos_um_sint_any" = df_selected$teve_ao_menos_um_sint_any,
      "mais_1_sint_any" = df_selected$mais_2_sint_any
    )
  )

# view table
df_selected_Table

df_selected <- probspace(df_selected)

df_selected

# Probability of the individual to show more than 2 severe symptoms given that they showed at least
# severe symptom
prob(df_selected,
     event = mais_2_sint_any == "teve",
     given = teve_ao_menos_um_sint_any == "teve_sintoma_any")

# resposta = 0.2619048 (26%)
