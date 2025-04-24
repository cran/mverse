## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, fig.width = 7
)
Sys.setenv(LANG = "en")

## ----load, warning=FALSE, message=FALSE---------------------------------------
library(mverse)

## -----------------------------------------------------------------------------
dplyr::glimpse(MASS::Boston) # using kable for displaying data in html

## ----lm-----------------------------------------------------------------------
mv <- create_multiverse(MASS::Boston)

## -----------------------------------------------------------------------------
formulas <- formula_branch(medv ~ log(lstat) * rm,
                           medv ~ log(lstat) * tax,
                           medv ~ log(lstat) * tax * rm)

## -----------------------------------------------------------------------------
mv <- mv |> add_formula_branch(formulas)

## -----------------------------------------------------------------------------
lm_mverse(mv)

## ----summary_lm---------------------------------------------------------------
summary(mv)

## -----------------------------------------------------------------------------
summary(mv, output = "df")

## -----------------------------------------------------------------------------
summary(mv, output = "f")

## -----------------------------------------------------------------------------
# output R-squared by `r.squared` or "r"
summary(mv, output = "r")

## ----fig.height=5-------------------------------------------------------------
spec_summary(mv, var = "log(lstat)") |>
  spec_curve(label = "code") +
  ggplot2::labs("Significant at 0.05")

