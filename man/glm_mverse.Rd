% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linear_models.R
\name{glm_mverse}
\alias{glm_mverse}
\title{Fit generalized linear regression models across the multiverse.}
\usage{
glm_mverse(.mverse, parallel = FALSE, progress = FALSE)
}
\arguments{
\item{.mverse}{a \code{mverse} object.}

\item{parallel}{passed to \code{multiverse::execute_multiverse()} to indicate
whether to execute the multiverse analysis in parallel. Defaults to FALSE.}

\item{progress}{passed to \code{multiverse::execute_multiverse()} to indicate
whether to include a progress bar for each step of the execution. Defaults
to FALSE.}
}
\value{
A \code{mverse} object with \code{glm} fitted.
}
\description{
\code{glm_mverse} fits \code{glm} across the multiverse
according to model specifications provided by \code{formula_branch}.
At least one \code{formula_branch} must have been added.
You can also specify the underlying error distribution and
the link function by adding a \code{family_branch}. If no
\code{family_branch} has been provided, it follows
the default behaviour of \code{glm} using the Gaussian
distribution with an identity link.
}
\examples{
\donttest{

# Fitting \code{glm} models across a multiverse.
hurricane_strength <- mutate_branch(
  NDAM,
  HighestWindSpeed,
  Minpressure_Updated_2014
)
hurricane_outliers <- filter_branch(
  !Name \%in\% c("Katrina", "Audrey", "Andrew"),
  TRUE # include all
)
model_specifications <- formula_branch(
  alldeaths ~ MasFem,
  alldeaths ~ MasFem + hurricane_strength
)
model_distributions <- family_branch(poisson)
mv <- create_multiverse(hurricane) \%>\%
  add_filter_branch(hurricane_outliers) \%>\%
  add_mutate_branch(hurricane_strength) \%>\%
  add_formula_branch(model_specifications) \%>\%
  add_family_branch(model_distributions) \%>\%
  glm_mverse()
}
}
\seealso{
Other model fitting functions: 
\code{\link{glm.nb_mverse}()},
\code{\link{lm_mverse}()}
}
\concept{model fitting functions}
