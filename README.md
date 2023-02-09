<!-- badges: start -->
[![R-CMD-check](https://github.com/lauken13/mrpkit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lauken13/mrpkit/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/lauken13/mrpkit/branch/master/graph/badge.svg)](https://app.codecov.io/gh/lauken13/mrpkit?branch=master)
<!-- badges: end -->

**NOTE: This package is still a work in progress and is yet not released or officially supported**

# mrpkit

For students and researchers who are comfortable with, at least, `lm()`, and want to conduct multilevel regression with post-stratification, `mrpkit` provides a reproducible, opinionated, workflow. Unlike writing all the code yourself, using mrpkit proactively addresses many common issues, and makes it possible for folks who are new to MRP to quickly conduct their first analysis.

This package implements a structured workflow for Multilevel Regression and Poststratification (MRP). The package assists in setting up the survey data and relationships between different variables in the sample and the population. From there, a substantial amount of data cleaning is automated, saving time and reducing the risk of coding errors. The package has native support for multilevel binomial and bernoulli models fit with lme4 and Stan (via brms and rstanarm) and also allows for the use of custom modeling functions. Summaries and simple visualizations of the resulting post-stratified estimates are provided.

### License 

Licensed under an MIT license. See the `LICENSE.md` file.
