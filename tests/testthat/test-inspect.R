test_that("summary() prints the multiverse table for a mverse object.", {
  mydf <- data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )
  mv <- mverse(mydf)
  mv %>%
    add_mutate_branch(
      mutate_branch(x + y, x - y, x * y, name = "m1"),
      mutate_branch(x + y, x - y, x * y, name = "m2")
    ) %>%
    add_filter_branch(
      filter_branch(x < 0, x > 0, x == 0, name = "f1"),
      filter_branch(x > 0, x < 0, x == 0, name = "f2")
    )
  mtable <- summary(mv)
  mtable_expected <- tibble::tibble(
    universe = factor(1:(3^4)),
    m1_branch = factor(rep(c("m1_1", "m1_2", "m1_3"), each = 3^3)),
    m2_branch = factor(rep(rep(c("m2_1", "m2_2", "m2_3"), each = 3^2), 3)),
    f1_branch = factor(rep(rep(c("f1_1", "f1_2", "f1_3"), each = 3), 3^2)),
    f2_branch = factor(rep(c("f2_1", "f2_2", "f2_3"), 3^3)),
    m1_branch_code = factor(
      rep(c("x + y", "x - y", "x * y"), each = 3^3),
      levels = c("x + y", "x - y", "x * y")
    ),
    m2_branch_code = factor(
      rep(rep(c("x + y", "x - y", "x * y"), each = 3^2), 3),
      levels = c("x + y", "x - y", "x * y")
    ),
    f1_branch_code = factor(
      rep(rep(c("x < 0", "x > 0", "x == 0"), each = 3), 3^2),
      levels = c("x < 0", "x > 0", "x == 0")
    ),
    f2_branch_code = factor(
      rep(c("x > 0", "x < 0", "x == 0"), 3^3),
      levels = c("x > 0", "x < 0", "x == 0")
    )
  )
  expect_equal(nrow(mtable), 3^4)
  expect_equal(ncol(mtable), 9)
  expect_equal(
    names(mtable),
    c("universe",
      c("m1_branch", "m2_branch",
        "f1_branch", "f2_branch",
        "m1_branch_code", "m2_branch_code",
        "f1_branch_code", "f2_branch_code")
    )
  )
  expect_equal(mtable, mtable_expected)
})

test_that("summary() doesn't truncate long branch options.", {
  mydf <- data.frame(col1 = c(1, 2, 3), col2 = 4:6, col3 = 7:9)
  mbranch <- mutate_branch(
    dplyr::if_else(col1 > 1, "a", dplyr::if_else(col1 == 1, "b", "c"))
  )
  fbranch <- formula_branch(
    cbind(col1, col2 - col1) ~ col3 + col3^2 +
      col3^3 + col3^4 + exp(col3 + col3^2),
    cbind(col1, col2 - col1) ~ col3 + col3^2 +
      col3^3 + col3^4 + exp(col3) + exp(col3^2)
  )
  mv <- mverse(mydf)
  add_mutate_branch(mv, mbranch)
  add_formula_branch(mv, fbranch)
  expect_true(any(stringr::str_detect(
    sapply(unlist(summary(mv)), as.character),
    "dplyr::if_else\\(col1 > 1,"
  )))
  expect_true(any(
    stringr::str_detect(
      sapply(unlist(summary(mv)), as.character),
      "cbind\\(col1, col2 - col1\\)"
    )
  ))
})

test_that("Universe is a categorical variable in the mutiverse table.", {
  mydf <- data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )
  mv <- mverse(mydf)
  mv %>%
    add_mutate_branch(
      mutate_branch(x + y, x - y, x * y, name = "m1"),
      mutate_branch(x + y, x - y, x * y, name = "m2")
    ) %>%
    add_filter_branch(
      filter_branch(x > 0, x < 0, x == 0, name = "f1"),
      filter_branch(x > 0, x < 0, x == 0, name = "f2")
    )
  mtable <- summary(mv)
  expect_true(is.factor(mtable$universe))
})

test_that(
  "summary.lm_mverse() outputs coefficient estimates with 95% confidence
  intervals by default.", {
    n <- 10
    mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
      dplyr::mutate(y = rnorm(n, x1 + x2))
    model_spec <- formula_branch(y ~ 0, y ~ 1, y ~ ., y ~ x1 * x2)
    mv <- mverse(mydf) %>%
      add_formula_branch(model_spec) %>%
      lm_mverse()
    smanu <- do.call(dplyr::bind_rows, list(
      tibble::tibble(
        term = "(None)", estimate = NA, std.error = NA,
        statistic = NA, p.value = NA, conf.low = NA, conf.high = NA
      ),
      lm(y ~ 1, data = mydf) %>% broom::tidy(conf.int = TRUE),
      lm(y ~ ., data = mydf) %>% broom::tidy(conf.int = TRUE),
      lm(y ~ x1 * x2, data = mydf) %>% broom::tidy(conf.int = TRUE)
    ))
    smverse <- summary(mv) %>%
      dplyr::select(-c(universe, model_spec_branch, model_spec_branch_code))
    expect_equal(smverse, smanu)
  }
)

test_that(
  "summary.glm_mverse() outputs coefficient estimates with 95% confidence
  intervals by default.", {
    x <- rnorm(100)
    y <- rbinom(100, 1, 1 / (1 + exp(-x)))
    mydf <- data.frame(x = x, y = y)
    frml <- formula_branch(y ~ 0, y ~ 1, y ~ x)
    fml <- family_branch(binomial)
    mv <- mverse(mydf) %>%
      add_formula_branch(frml) %>%
      add_family_branch(fml) %>%
      glm_mverse()
    smanu <- do.call(dplyr::bind_rows, list(
      tibble::tibble(
        term = "(None)", estimate = NA, std.error = NA,
        statistic = NA, p.value = NA, conf.low = NA, conf.high = NA
      ),
      glm(y ~ 1, data = mydf, family = binomial) %>%
        broom::tidy(conf.int = TRUE),
      glm(y ~ x, data = mydf, family = binomial) %>%
        broom::tidy(conf.int = TRUE)
    ))
    smverse <- summary(mv) %>%
      dplyr::select(-c(
        universe,
        frml_branch,
        frml_branch_code,
        fml_branch,
        fml_branch_code
      ))
    expect_equal(smverse, smanu)
  }
)

test_that(
  "summary.glm.nb_mverse() outputs coefficient estimates with 95% confidence
  intervals by default.", {
    x <- rep(1:2, 50)
    y <- rnbinom(100, 1, .5)
    mydf <- data.frame(x = x, y = y)
    frml <- formula_branch(y ~ 0, y ~ 1, y ~ x)
    mv <- mverse(mydf) %>%
      add_formula_branch(frml) %>%
      glm.nb_mverse()
    smanu <- do.call(dplyr::bind_rows, list(
      tibble::tibble(
        term = "(None)", estimate = NA, std.error = NA,
        statistic = NA, p.value = NA, conf.low = NA, conf.high = NA
      ),
      MASS::glm.nb(y ~ 1, data = mydf) %>% broom::tidy(conf.int = TRUE),
      MASS::glm.nb(y ~ x, data = mydf) %>% broom::tidy(conf.int = TRUE)
    ))
    smverse <- summary(mv) %>%
      dplyr::select(-c(
        universe,
        frml_branch,
        frml_branch_code
      ))
    expect_equal(smverse, smanu)
  }
)

test_that(
  "summary.lm_mverse(conf.int = FALSE) outputs coefficient estimates without
  any confidence intervals.", {
    n <- 10
    mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
      dplyr::mutate(y = rnorm(n, x1 + x2))
    model_spec <- formula_branch(y ~ 0, y ~ 1, y ~ ., y ~ x1 * x2)
    mv <- mverse(mydf) %>%
      add_formula_branch(model_spec) %>%
      lm_mverse()
    smanu <- do.call(dplyr::bind_rows, list(
      tibble::tibble(
        term = "(None)", estimate = NA, std.error = NA,
        statistic = NA, p.value = NA
      ),
      lm(y ~ 1, data = mydf) %>% broom::tidy(),
      lm(y ~ ., data = mydf) %>% broom::tidy(),
      lm(y ~ x1 * x2, data = mydf) %>% broom::tidy()
    ))
    smverse <- summary(mv, conf.int = FALSE) %>%
      dplyr::select(-c(universe, model_spec_branch, model_spec_branch_code))
    expect_equal(smverse, smanu)
  }
)

test_that(
  "summary.glm_mverse(conf.int = FALSE) outputs coefficient estimates
  without any confidence intervals.", {
    x <- rnorm(100)
    y <- rbinom(100, 1, 1 / (1 + exp(-x)))
    mydf <- data.frame(x = x, y = y)
    frml <- formula_branch(y ~ 0, y ~ 1, y ~ x)
    fml <- family_branch(binomial)
    mv <- mverse(mydf) %>%
      add_formula_branch(frml) %>%
      add_family_branch(fml) %>%
      glm_mverse()
    smanu <- do.call(dplyr::bind_rows, list(
      tibble::tibble(
        term = "(None)", estimate = NA, std.error = NA,
        statistic = NA, p.value = NA
      ),
      glm(y ~ 1, data = mydf, family = binomial) %>%
        broom::tidy(conf.int = FALSE),
      glm(y ~ x, data = mydf, family = binomial) %>%
        broom::tidy(conf.int = FALSE)
    ))
    smverse <- summary(mv, conf.int = FALSE) %>%
      dplyr::select(-c(
        universe,
        frml_branch,
        frml_branch_code,
        fml_branch,
        fml_branch_code
      ))
    expect_equal(smverse, smanu)
  }
)

test_that(
  "summary.glm.nb_mverse(conf.int = FALSE) outputs coefficient estimates
  without any confidence intervals.", {
    x <- rep(1:2, 50)
    y <- rnbinom(100, 1, .5)
    mydf <- data.frame(x = x, y = y)
    frml <- formula_branch(y ~ 0, y ~ 1, y ~ x)
    mv <- mverse(mydf) %>%
      add_formula_branch(frml) %>%
      glm.nb_mverse()
    smanu <- do.call(dplyr::bind_rows, list(
      tibble::tibble(
        term = "(None)", estimate = NA, std.error = NA,
        statistic = NA, p.value = NA
      ),
      MASS::glm.nb(y ~ 1, data = mydf) %>% broom::tidy(conf.int = FALSE),
      MASS::glm.nb(y ~ x, data = mydf) %>% broom::tidy(conf.int = FALSE)
    ))
    smverse <- summary(mv, conf.int = FALSE) %>%
      dplyr::select(-c(
        universe,
        frml_branch,
        frml_branch_code
      ))
    expect_equal(smverse, smanu)
  }
)

test_that("summary.lm_mverse(output = 'df') outputs degrees of freedom.", {
  n <- 10
  mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
    dplyr::mutate(y = rnorm(n, x1 + x2))
  model_spec <- formula_branch(y ~ 0, y ~ 1, y ~ ., y ~ x1 * x2)
  mv <- mverse(mydf) %>%
    add_formula_branch(model_spec) %>%
    lm_mverse()
  smanu <- do.call(dplyr::bind_rows, list(
    as.data.frame(t(summary(lm(y ~ 0, data = mydf))$df)) %>%
      dplyr::rename(p = V1, n.minus.p = V2, p.star = V3),
    as.data.frame(t(summary(lm(y ~ 1, data = mydf))$df)) %>%
      dplyr::rename(p = V1, n.minus.p = V2, p.star = V3),
    as.data.frame(t(summary(lm(y ~ ., data = mydf))$df)) %>%
      dplyr::rename(p = V1, n.minus.p = V2, p.star = V3),
    as.data.frame(t(summary(lm(y ~ x1 * x2, data = mydf))$df)) %>%
      dplyr::rename(p = V1, n.minus.p = V2, p.star = V3)
  ))
  smverse2 <- summary(mv, output = "df") %>%
    dplyr::select(-c(universe, model_spec_branch, model_spec_branch_code))
  expect_equal(smverse2, smanu)
})

test_that("summary.glm_mverse(output = 'df') outputs degrees of freedom.", {
    x <- rnorm(100)
    y <- rbinom(100, 1, 1 / (1 + exp(-x)))
    mydf <- data.frame(x = x, y = y)
    frml <- formula_branch(y ~ 0, y ~ 1, y ~ x)
    fml <- family_branch(binomial)
    mv <- mverse(mydf) %>%
      add_formula_branch(frml) %>%
      add_family_branch(fml) %>%
      glm_mverse()
    smanu <- data.frame(rbind(
      summary(glm(y ~ 0, data = mydf, family = binomial))$df,
      summary(glm(y ~ 1, data = mydf, family = binomial))$df,
      summary(glm(y ~ x, data = mydf, family = binomial))$df
    ))
    names(smanu) <- c("rank", "df.residual", "n.coef")
    smverse <- summary(mv, output = "df") %>%
      dplyr::select(-c(
        universe,
        frml_branch,
        frml_branch_code,
        fml_branch,
        fml_branch_code
      ))
    expect_equal(smverse, smanu)
  }
)

test_that("summary.glm.nb_mverse(output = 'df') outputs degrees of freedom.", {
    x <- rep(1:2, 50)
    y <- rnbinom(100, 1, .5)
    mydf <- data.frame(x = x, y = y)
    frml <- formula_branch(y ~ 0, y ~ 1, y ~ x)
    mv <- mverse(mydf) %>%
      add_formula_branch(frml) %>%
      glm.nb_mverse()
    smanu <- data.frame(rbind(
      summary(MASS::glm.nb(y ~ 0, data = mydf))$df,
      summary(MASS::glm.nb(y ~ 1, data = mydf))$df,
      summary(MASS::glm.nb(y ~ x, data = mydf))$df
    ))
    names(smanu) <- c("rank", "df.residual", "n.coef")
    smverse <- summary(mv, output = "df") %>%
      dplyr::select(-c(
        universe,
        frml_branch,
        frml_branch_code
      ))
    expect_equal(smverse, smanu)
  }
)

test_that("summary.lm_mverse(output = 'r') outputs R squared values.", {
  n <- 10
  mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
    dplyr::mutate(y = rnorm(n, x1 + x2))
  model_spec <- formula_branch(y ~ 0, y ~ 1, y ~ ., y ~ x1 * x2)
  mv <- mverse(mydf) %>%
    add_formula_branch(model_spec) %>%
    lm_mverse()
  smanu <- do.call(dplyr::bind_rows, list(
    as.data.frame(t(c(
      summary(lm(y ~ 0, data = mydf))$r.squared,
      summary(lm(y ~ 0, data = mydf))$adj.r.squared
    ))) %>%
      dplyr::rename(r.squared = V1, adj.r.squared = V2),
    as.data.frame(t(c(
      summary(lm(y ~ 1, data = mydf))$r.squared,
      summary(lm(y ~ 1, data = mydf))$adj.r.squared
    ))) %>%
      dplyr::rename(r.squared = V1, adj.r.squared = V2),
    as.data.frame(t(c(
      summary(lm(y ~ ., data = mydf))$r.squared,
      summary(lm(y ~ ., data = mydf))$adj.r.squared
    ))) %>%
      dplyr::rename(r.squared = V1, adj.r.squared = V2),
    as.data.frame(t(c(
      summary(lm(y ~ x1 * x2, data = mydf))$r.squared,
      summary(lm(y ~ x1 * x2, data = mydf))$adj.r.squared
    ))) %>%
      dplyr::rename(r.squared = V1, adj.r.squared = V2)
  ))
  smverse <- summary(mv, output = "r") %>%
    dplyr::select(-c(universe, model_spec_branch, model_spec_branch_code))
  smverse2 <- summary(mv, output = "r.squared") %>%
    dplyr::select(-c(universe, model_spec_branch, model_spec_branch_code))
  expect_equal(smverse, smanu)
  expect_equal(smverse2, smanu)
})

test_that("summary.lm_mverse(output = 'f') outputs F statistics.", {
  n <- 10
  mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
    dplyr::mutate(y = rnorm(n, x1 + x2))
  model_spec <- formula_branch(y ~ 0, y ~ 1, y ~ ., y ~ x1 * x2)
  mv <- mverse(mydf) %>%
    add_formula_branch(model_spec) %>%
    lm_mverse()
  smanu <- do.call(dplyr::bind_rows, list(
    data.frame(value = NA, numdf = NA, dendf = NA),
    data.frame(value = NA, numdf = NA, dendf = NA),
    as.data.frame(t(summary(lm(y ~ ., data = mydf))$fstatistic)),
    as.data.frame(t(summary(lm(y ~ x1 * x2, data = mydf))$fstatistic))
  ))
  smverse <- summary(mv, output = "f") %>%
    dplyr::select(-c(universe, model_spec_branch, model_spec_branch_code))
  smverse2 <- summary(mv, output = "fstatistic") %>%
    dplyr::select(-c(universe, model_spec_branch, model_spec_branch_code))
  expect_equal(smverse, smanu)
  expect_equal(smverse2, smanu)
})

test_that("summary.glm_mverse(output = 'de') outputs deviance.", {
    x <- rnorm(100)
    y <- rbinom(100, 1, 1 / (1 + exp(-x)))
    mydf <- data.frame(x = x, y = y)
    frml <- formula_branch(y ~ 0, y ~ 1, y ~ x)
    fml <- family_branch(binomial)
    mv <- mverse(mydf) %>%
      add_formula_branch(frml) %>%
      add_family_branch(fml) %>%
      glm_mverse()
    smanu <- rbind(
      c(
        deviance = summary(
          glm(y ~ 0, data = mydf, family = binomial)
        )$deviance,
        null.deviance = summary(
          glm(y ~ 0, data = mydf, family = binomial)
        )$null.deviance
      ),
      c(
        deviance = summary(
          glm(y ~ 1, data = mydf, family = binomial)
        )$deviance,
        null.deviance = summary(
          glm(y ~ 1, data = mydf, family = binomial)
        )$null.deviance
      ),
      c(
        deviance = summary(
          glm(y ~ x, data = mydf, family = binomial)
        )$deviance,
        null.deviance = summary(
          glm(y ~ x, data = mydf, family = binomial)
        )$null.deviance
      )
    )
    smverse <- summary(mv, output = "de") %>%
      dplyr::select(-c(
        universe,
        frml_branch,
        frml_branch_code,
        fml_branch,
        fml_branch_code
      ))
    expect_equal(as.matrix(smverse), smanu)
  }
)

test_that("summary.glm.nb_mverse(output = 'de') outputs deviance.", {
    x <- rep(1:2, 50)
    y <- rnbinom(100, 1, .5)
    mydf <- data.frame(x = x, y = y)
    frml <- formula_branch(y ~ 0, y ~ 1, y ~ x)
    mv <- mverse(mydf) %>%
      add_formula_branch(frml) %>%
      glm.nb_mverse()
    smanu <- rbind(
      c(
        deviance = summary(MASS::glm.nb(y ~ 0, data = mydf))$deviance,
        null.deviance = summary(MASS::glm.nb(y ~ 0, data = mydf))$null.deviance
      ),
      c(
        deviance = summary(MASS::glm.nb(y ~ 1, data = mydf))$deviance,
        null.deviance = summary(MASS::glm.nb(y ~ 1, data = mydf))$null.deviance
      ),
      c(
        deviance = summary(MASS::glm.nb(y ~ x, data = mydf))$deviance,
        null.deviance = summary(MASS::glm.nb(y ~ x, data = mydf))$null.deviance
      )
    )
    smverse <- summary(mv, output = "de") %>%
      dplyr::select(-c(
        universe,
        frml_branch,
        frml_branch_code
      ))
    expect_equal(as.matrix(smverse), smanu)
  }
)

test_that("summary.lm_mverse(output = 'aic') outputs AIC and BIC statistics.", {
  n <- 10
  mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
    dplyr::mutate(y = rnorm(n, x1 + x2))
  model_spec <- formula_branch(y ~ 0, y ~ 1, y ~ ., y ~ x1 * x2)
  mv <- mverse(mydf) %>%
    add_formula_branch(model_spec) %>%
    lm_mverse()
  smanu <- do.call(rbind, lapply(
    list(
      lm(y ~ 0, data = mydf),
      lm(y ~ 1, data = mydf),
      lm(y ~ ., data = mydf),
      lm(y ~ x1 * x2, data = mydf)
    ),
    function(x) {
      c(
        AIC = stats::AIC(x),
        BIC = stats::BIC(x)
      )
    }
  ))
  smverse <- summary(mv, output = "aic") %>%
    dplyr::select(-c(universe, model_spec_branch, model_spec_branch_code))
  smverse2 <- summary(mv, output = "bic") %>%
    dplyr::select(-c(universe, model_spec_branch, model_spec_branch_code))
  expect_equal(as.matrix(smverse), smanu)
  expect_equal(as.matrix(smverse2), smanu)
})


test_that("summary.glm_mverse(output = 'aic') outputs AIC and BIC statistics.", {
    x <- rnorm(100)
    y <- rbinom(100, 1, 1 / (1 + exp(-x)))
    mydf <- data.frame(x = x, y = y)
    frml <- formula_branch(y ~ 0, y ~ 1, y ~ x)
    fml <- family_branch(binomial)
    mv <- mverse(mydf) %>%
      add_formula_branch(frml) %>%
      add_family_branch(fml) %>%
      glm_mverse()
    smanu <- do.call(rbind, lapply(
      list(
        glm(y ~ 0, data = mydf, family = binomial),
        glm(y ~ 1, data = mydf, family = binomial),
        glm(y ~ x, data = mydf, family = binomial)
      ),
      function(x) {
        c(
          AIC = stats::AIC(x),
          BIC = stats::BIC(x)
        )
      }
    ))
    smverse <- summary(mv, output = "aic") %>%
      dplyr::select(-c(
        universe,
        frml_branch,
        frml_branch_code,
        fml_branch,
        fml_branch_code
      ))
    smverse2 <- summary(mv, output = "bic") %>%
      dplyr::select(-c(
        universe,
        frml_branch,
        frml_branch_code,
        fml_branch,
        fml_branch_code
      ))
    expect_equal(as.matrix(smverse), smanu)
    expect_equal(as.matrix(smverse2), smanu)
  }
)

test_that("summary.glm.nb_mverse(output = 'aic') outputs AIC and BIC statistics.", {
    x <- rep(1:2, 50)
    y <- rnbinom(100, 1, .5)
    mydf <- data.frame(x = x, y = y)
    frml <- formula_branch(y ~ 0, y ~ 1, y ~ x)
    mv <- mverse(mydf) %>%
      add_formula_branch(frml) %>%
      glm.nb_mverse()
    smanu <-do.call(rbind, lapply(
      list(
        MASS::glm.nb(y ~ 0, data = mydf),
        MASS::glm.nb(y ~ 1, data = mydf),
        MASS::glm.nb(y ~ x, data = mydf)
      ),
      function(x) {
        c(
          AIC = stats::AIC(x),
          BIC = stats::BIC(x)
        )
      }
    ))
    smverse <- summary(mv, output = "aic") %>%
      dplyr::select(-c(
        universe,
        frml_branch,
        frml_branch_code
      ))
    smverse2 <- summary(mv, output = "bic") %>%
      dplyr::select(-c(
        universe,
        frml_branch,
        frml_branch_code
      ))
    expect_equal(as.matrix(smverse), smanu)
    expect_equal(as.matrix(smverse2), smanu)
  }
)

test_that(
  "summary.lm_mverse(output = 'x') throws an invalid output argument error.", {
    n <- 10
    mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
      dplyr::mutate(y = rnorm(n, x1 + x2))
    model_spec <- formula_branch(y ~ 0, y ~ 1, y ~ ., y ~ x1 * x2)
    mv <- mverse(mydf) %>%
      add_formula_branch(model_spec) %>%
      lm_mverse()
    expect_error(
      summary(mv, output = "x"),
      "Invalid output argument."
    )
  }
)

test_that(
  "summary.glm_mverse(output = 'x') throws an invalid output argument error.", {
    x <- rnorm(100)
    y <- rbinom(100, 1, 1 / (1 + exp(-x)))
    mydf <- data.frame(x = x, y = y)
    frml <- formula_branch(y ~ 0, y ~ 1, y ~ x)
    fml <- family_branch(binomial)
    mv <- mverse(mydf) %>%
      add_formula_branch(frml) %>%
      add_family_branch(fml) %>%
      glm_mverse()
    expect_error(
      summary(mv, output = "x"),
      "Invalid output argument."
    )
  }
)

test_that(
  "summary.glm.nb_mverse(output = 'x') throws an invalid output argument error.", {
    x <- rep(1:2, 50)
    y <- rnbinom(100, 1, .5)
    mydf <- data.frame(x = x, y = y)
    frml <- formula_branch(y ~ 0, y ~ 1, y ~ x)
    mv <- mverse(mydf) %>%
      add_formula_branch(frml) %>%
      glm.nb_mverse()
    expect_error(
      summary(mv, output = "x"),
      "Invalid output argument."
    )
  }
)

test_that("multiverse_tree() expects at least one branch.", {
  mydf <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
  mv <- mverse(mydf)
  expect_error(multiverse_tree(mv), "No branch to display in a tree.")
})

test_that("multiverse_tree() outputs a ggplot object.", {
  z <- mutate_branch(x, y, name = "z")
  w <- mutate_branch(x + y, x - y, name = "w")
  mydf <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
  mv <- mverse(mydf) %>%
    add_mutate_branch(z, w)
  mtree <- multiverse_tree(mv)
  expect_s3_class(mtree, "ggplot")
})

test_that("multiverse_tree() draws a graph with nodes and edges.", {
  z <- mutate_branch(x, y, name = "z")
  w <- mutate_branch(x + y, x - y, name = "w")
  mydf <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
  mv <- mverse(mydf) %>%
    add_mutate_branch(z, w)
  mtree <- multiverse_tree(mv)
  expect_s3_class(mtree$layers[[1]]$geom, "GeomEdgePath")
  expect_s3_class(mtree$layers[[2]]$geom, "GeomPoint")
})

test_that("multiverse_tree() draws a graph with correct data.", {
  z <- mutate_branch(x, y, name = "z")
  w <- mutate_branch(x + y, x - y, name = "w")
  mydf <- data.frame(
    u = c(0, 1, 0),
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )
  mv <- mverse(mydf) %>%
    add_mutate_branch(z, w)
  mtree <- multiverse_tree(mv)
  mtree_name <- multiverse_tree(mv, label = "name")
  mtree_code <- multiverse_tree(mv, label = "code")
  expect_equal(nrow(mtree$data), 7)
  expect_equal(nrow(mtree_name$data), 7)
  expect_equal(nrow(mtree_code$data), 7)
  expect_setequal(
    mtree$data$name,
    c("Data", "z_1", "z_2", "z_1_w_1", "z_1_w_2", "z_2_w_1", "z_2_w_2")
  )
  expect_setequal(
    mtree_name$data$name,
    c("Data", "z_1", "z_2", "z_1_w_1", "z_1_w_2", "z_2_w_1", "z_2_w_2")
  )
  expect_setequal(
    mtree_code$data$name,
    c("Data", "x", "y", "x_x + y", "x_x - y", "y_x + y", "y_x - y")
  )
  f <- formula_branch(y ~ u, covariates = c("z", "w"))
  mv <- mverse(mydf) %>%
    add_mutate_branch(z, w) %>%
    add_formula_branch(f)
  mtree <- multiverse_tree(mv, label = "code")
  mtree_partial <- multiverse_tree(mv, label = "name",
                                   branches = c("f", "covariate_z"))
  expect_equal(nrow(mtree$data), 35)
  expect_equal(nrow(mtree_partial$data), 4)
  expect_setequal(
    mtree_partial$data$name,
    c("Data", "f_1", "f_1_include_z", "f_1_exclude_z")
  )
})

test_that("multiverse_tree() runs after fitting a lm model.", {
  mydf <- data.frame(x = sample.int(25), y = sample.int(25), u = sample.int(25))
  w <- mutate_branch(x + y + u, x - y + u)
  z <- mutate_branch(x + y < mean(w), x + y > mean(w))
  frml <- formula_branch(w ~ x, covariates = c("y"))
  mv <- mverse(mydf) %>%
    add_mutate_branch(w) %>%
    add_formula_branch(frml)
  mtree <- multiverse_tree(mv)
  mv <- mv %>%
    lm_mverse()
  mtree_lm <- multiverse_tree(mv)
  expect_true(ggplot2::is_ggplot(mtree))
  expect_equal(mtree$data$.ggraph.index, mtree_lm$data$.ggraph.index)
  expect_equal(mtree$data$.ggraph.orig_index, mtree_lm$data$.ggraph.orig_index)
})

test_that("multiverse_tree() runs after fitting a glm model.", {
  mydf <- data.frame(x = rnorm(100), y = sample.int(100) - 50)
  p1 <- mutate_branch(1 / (1 + exp(-(x + y / 100))))
  p2 <- mutate_branch(1 / (1 + exp(-(x - y / 100))))
  z <- mutate_branch(rbinom(100, 1, p1), rbinom(100, 1, p2))
  frml <- formula_branch(z ~ x, covariates = "y")
  fml <- family_branch(binomial)
  mv <- mverse(mydf) %>%
    add_mutate_branch(p1, p2, z) %>%
    add_formula_branch(frml) %>%
    add_family_branch(fml)
  mtree <- multiverse_tree(mv)
  mv <- mv %>%
    glm_mverse()
  mtree_glm <- multiverse_tree(mv)
  expect_true(ggplot2::is_ggplot(mtree))
  expect_equal(mtree$data$.ggraph.index, mtree_glm$data$.ggraph.index)
  expect_equal(mtree$data$.ggraph.orig_index, mtree_glm$data$.ggraph.orig_index)
})
