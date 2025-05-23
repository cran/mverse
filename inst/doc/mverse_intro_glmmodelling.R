## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,  fig.width = 7, fig.height = 4
)

## ----load_package, warning=FALSE, message=FALSE-------------------------------
library(mverse)
library(dplyr)
library(ggplot2)

## ----load_data, warning=FALSE, message=FALSE----------------------------------
glimpse(hurricane)

## ----create_object------------------------------------------------------------
hurricane_mv <- create_multiverse(hurricane)

## -----------------------------------------------------------------------------
hurricane |>
  ggplot(aes(alldeaths)) +
  geom_histogram(bins = 25) +
  stat_bin(
    aes(label = after_stat(count)), bins = 25,
    geom = "text", vjust = -.7, size = 2
  )

## -----------------------------------------------------------------------------
hurricane |>
  filter(alldeaths > median(alldeaths)) |>
  arrange(desc(alldeaths)) |>
  select(Name, alldeaths) |>
  head()

## ----filter_branch------------------------------------------------------------
death_outliers <- filter_branch(
  none = TRUE,
  Katrina = Name != "Katrina",
  KatrinaAudrey = !(Name %in% c("Katrina", "Audrey"))
)

## -----------------------------------------------------------------------------
hurricane_mv <- hurricane_mv |> add_filter_branch(death_outliers)

summary(hurricane_mv)

## ----mutate_branch------------------------------------------------------------
femininity <- mutate_branch(binary = Gender_MF,
                            continuous = MasFem)

## -----------------------------------------------------------------------------
damage <- mutate_branch(original = NDAM,
                        log = log(NDAM))

## -----------------------------------------------------------------------------
hurricane_mv <- hurricane_mv |> add_mutate_branch(femininity, damage)

summary(hurricane_mv)

## -----------------------------------------------------------------------------
models <- formula_branch(alldeaths ~ damage + femininity)

hurricane_mv <- hurricane_mv |> add_formula_branch(models)

summary(hurricane_mv)

## ----modelling_branch---------------------------------------------------------
distributions <- family_branch(poisson, gaussian)

## -----------------------------------------------------------------------------
hurricane_mv <- hurricane_mv |> add_family_branch(distributions)

summary(hurricane_mv)

## -----------------------------------------------------------------------------
multiverse_tree(hurricane_mv, label = "code", label_size = 4,
                branches = c("models", "distributions"))

## -----------------------------------------------------------------------------
glm_mverse(hurricane_mv)

glm_summary <- summary(hurricane_mv)

glm_summary

## ----fig.height=8-------------------------------------------------------------
spec_summary(hurricane_mv, var = "femininity") |>
  spec_curve(label = "code", spec_matrix_spacing = 4) +
  labs(colour = "Significant at 0.05") +
  theme(legend.position = "top")

## -----------------------------------------------------------------------------
hurricane |>
  ggplot(aes(sample = alldeaths)) +
  stat_qq() +
  stat_qq_line()

hurricane |>
  mutate(logd = log(alldeaths + 1)) |>
  ggplot(aes(sample = logd)) +
  stat_qq() +
  stat_qq_line()

## -----------------------------------------------------------------------------
hurricane_mv <- create_multiverse(hurricane)

dep_var <- mutate_branch(alldeaths, log(alldeaths + 1))

femininity <- mutate_branch(binary_gender = Gender_MF,
                            cts_gender = MasFem)

damage <- mutate_branch(damage_orig = NDAM,
                        damage_log = log(NDAM))

models <- formula_branch(dep_var ~ damage + femininity)

distributions <- family_branch(poisson, gaussian)

hurricane_mv <- hurricane_mv |>
  add_mutate_branch(dep_var, femininity, damage) |>
  add_formula_branch(models) |>
  add_family_branch(distributions)


## -----------------------------------------------------------------------------
multiverse_tree(hurricane_mv, label = "code", c("dep_var", "distributions"))

## -----------------------------------------------------------------------------
match_poisson <- branch_condition(alldeaths, poisson)

match_log_lin <- branch_condition(log(alldeaths + 1), gaussian)

hurricane_mv <- add_branch_condition(hurricane_mv, match_poisson, match_log_lin)

## -----------------------------------------------------------------------------
multiverse_tree(hurricane_mv, label = "code", c("dep_var", "distributions"))

## -----------------------------------------------------------------------------
glm_mverse(hurricane_mv)

summary(hurricane_mv)

## ----fig.height=8-------------------------------------------------------------
spec_summary(hurricane_mv, var = "femininity") |>
  spec_curve(label = "code", spec_matrix_spacing = 4) +
  labs(colour = "Significant at 0.05") +
  theme(legend.position = "top")

## -----------------------------------------------------------------------------
hurricane_nb_mv <- create_multiverse(hurricane)

femininity <- mutate_branch(binary_gender = Gender_MF,
                            cts_gender = MasFem)

damage <- mutate_branch(damage_orig = NDAM,
                        damage_log = log(NDAM))

models <- formula_branch(alldeaths ~ damage + femininity)

hurricane_nb_mv <- hurricane_nb_mv |>
  add_mutate_branch(femininity, damage) |>
  add_formula_branch(models)

## -----------------------------------------------------------------------------
summary(hurricane_nb_mv)

## -----------------------------------------------------------------------------
glm.nb_mverse(hurricane_nb_mv)

summary(hurricane_nb_mv)

## ----fig.height=8-------------------------------------------------------------
spec_summary(hurricane_nb_mv, var = "femininity") |>
  spec_curve(label = "code", spec_matrix_spacing = 4) +
  labs(colour = "Significant at 0.05") +
  theme(legend.position = "top")

