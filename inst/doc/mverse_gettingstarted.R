## ----setup, include = FALSE, message=FALSE, warning=FALSE---------------------
knitr::opts_chunk$set(collapse = TRUE, fig.width = 7, fig.height = 4)

## -----------------------------------------------------------------------------
library(mverse)
library(tibble)
library(dplyr)
library(ggplot2)
set.seed(6)
df <- tibble(col1 = rnorm(5, 0, 1), col2 = col1 + runif(5))

## -----------------------------------------------------------------------------
mv <- create_multiverse(df)

## -----------------------------------------------------------------------------
# Step 2: create a branch - each branch corresponds to a universe

transformation_branch <- mutate_branch(col1 = col1,
                                       col1_t1 = log(abs(col1 + 1)),
                                       col1_t2 = abs(col1))

## -----------------------------------------------------------------------------
mv <- mv |> add_mutate_branch(transformation_branch)

## -----------------------------------------------------------------------------
mv <- execute_multiverse(mv)

## ----echo=FALSE---------------------------------------------------------------
extract <- mverse::extract

## -----------------------------------------------------------------------------
df_transformed <- extract(mv)

df_transformed |> head()

## -----------------------------------------------------------------------------
df_transformed |>
  group_by(transformation_branch_branch) |>
  summarise(n = n(),
            mean = mean(transformation_branch),
            sd = sd(transformation_branch),
            median = median(transformation_branch),
            IQR = IQR(transformation_branch))

df_transformed |>
  ggplot(aes(x = transformation_branch)) + geom_histogram(bins = 3) +
  facet_wrap(vars(transformation_branch_branch))

## -----------------------------------------------------------------------------
mv1 <- create_multiverse(df)

## -----------------------------------------------------------------------------
formulas <- formula_branch(col2 ~ col1,
                           col2 ~ log(abs(col1 + 1)),
                           col2 ~ abs(col1))

## -----------------------------------------------------------------------------
mv1 <- mv1 |> add_formula_branch(formulas)

## -----------------------------------------------------------------------------
lm_mverse(mv1)

## -----------------------------------------------------------------------------
summary(mv1)

## -----------------------------------------------------------------------------
mod1 <- formula(col2 ~ col1)

mod2 <- formula(col2 ~ log(abs(col1 + 1)))

mod3 <- formula(col2 ~ abs(col1))

models <- list(mod1, mod2, mod3)

models |>
  purrr::map(lm, data = df) |>
  purrr::map(broom::tidy) |>
  bind_rows()

## -----------------------------------------------------------------------------
modfit <- lapply(models, function(x) lm(x, data = df))

lapply(modfit, function(x) summary(x)[4])

## ----load, message=FALSE------------------------------------------------------
library(mverse)
soccer_bias <- soccer[!is.na(soccer$rater1) & !is.na(soccer$rater2),
                      c("playerShort", "rater1", "rater2")]
soccer_bias <- unique(soccer_bias)
head(soccer_bias)

## ----base_r-------------------------------------------------------------------
skin_option_1 <- (soccer_bias$rater1 + soccer_bias$rater2) / 2
skin_option_2 <- ifelse(soccer_bias$rater1 > soccer_bias$rater2,
                        soccer_bias$rater1,
                        soccer_bias$rater2)
skin_option_3 <- ifelse(soccer_bias$rater1 < soccer_bias$rater2,
                        soccer_bias$rater1,
                        soccer_bias$rater2)
skin_option_4 <- soccer_bias$rater1
skin_option_5 <- soccer_bias$rater2

## ----hist_base----------------------------------------------------------------
library(ggplot2)
ggplot(mapping = aes(x = skin_option_1)) +
  geom_histogram(breaks = seq(0, 1, 0.2),
                 colour = "white") +
  labs(title = "Histogram of player skin tones (Option 1: Mean).",
       x = "Skin Tone", y = "Count")

## ----hist_base_overlaid-------------------------------------------------------
skin_option_all <- data.frame(
  x = c(skin_option_1,
        skin_option_2,
        skin_option_3,
        skin_option_4,
        skin_option_5),
  Option = rep(
    c("Option 1: Mean",
      "Option 2: Max",
      "Option 3: Min",
      "Option 4: Rater 1",
      "Option 5: Rater 2"),
    each = nrow(df)
  )
)
ggplot(data = skin_option_all) +
  geom_histogram(aes(x = x), binwidth = 0.1) +
  labs(title = "Histogram of player skin tones for each option.",
       x = "Skin Tone", y = "Count") +
  facet_wrap(. ~ Option)

## ----create_mv----------------------------------------------------------------
soccer_bias_mv <- create_multiverse(soccer_bias)

## ----mutate_branch------------------------------------------------------------
skin_tone <- mutate_branch(
  (rater1 + rater2) / 2,
  ifelse(rater1 > rater2, rater1, rater2),
  ifelse(rater1 < rater2, rater1, rater2),
  rater1,
  rater2
)

## ----add_vb-------------------------------------------------------------------
soccer_bias_mv <- soccer_bias_mv |> add_mutate_branch(skin_tone)

## ----check_multiverse---------------------------------------------------------
summary(soccer_bias_mv)

## ----exec---------------------------------------------------------------------
execute_multiverse(soccer_bias_mv)

## ----extract_multiverse-------------------------------------------------------
branched <- mverse::extract(soccer_bias_mv)

## ----head_skin_tone-----------------------------------------------------------
branched |>
  filter(skin_tone_branch == "(rater1 + rater2) / 2") |>
  head()

## -----------------------------------------------------------------------------
branched |>
  group_by(skin_tone_branch) |>
  summarise(n = n(),
            mean = mean(skin_tone),
            sd = sd(skin_tone),
            median = median(skin_tone),
            IQR = IQR(skin_tone))

## ----extract_fraction---------------------------------------------------------
frac <- extract(soccer_bias_mv, frow =  0.05)

## -----------------------------------------------------------------------------
frac |>
  group_by(universe) |>
  tally() |>
  mutate(percent = (n / sum(n)) * 100)

## ----compare_universe, warning=FALSE------------------------------------------
branched |>
  ggplot(mapping = aes(x = skin_tone, color = universe)) +
  geom_density(alpha = 0.2) +
  labs(title = "Density of player skin tones for each option.",
       x = "Skin Tone", y = "Density") +
  scale_color_discrete(
    labels = c("Option 1: Mean",
               "Option 2: Max",
               "Option 3: Min",
               "Option 4: Rater 1",
               "Option 5: Rater 2"),
    name = NULL
  )

## ----compare_universe_hist, warning=FALSE-------------------------------------
branched |>
  ggplot(mapping = aes(x = skin_tone)) +
  geom_histogram(position = "dodge", bins = 21) +
  labs(title = "Histogram of player skin tones for each option.",
       y = "Count", x = "Skin Tone") +
  facet_wrap(
    . ~ universe,
    labeller = labeller(
      universe = c(`1` = "Option 1: Mean",
                   `2` = "Option 2: Max",
                   `3` = "Option 3: Min",
                   `4` = "Option 4: Rater 1",
                   `5` = "Option 5: Rater 2")
    )
  )

