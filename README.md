
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GADGET: Gaussian Process Approximations for Designed Experiments

The `GADGET` package computes near-optimal Bayesian experimental designs
using Gaussian process optimization. At its core is the ability to
calculate static designs that maximize a design criterion that may be
either deterministic or stochastic. In particular, stochastic design
criteria could be a Monte Carlo estimates of an expected utility based
on MCMC posterior draws. `GADGET` utilizes the algorithm proposed by
Weaver et al. (2016) <doi:10.1214/15-BA945> and performs Gaussian
process validation using the statistics introduced by Bastos and O’Hagan
(2009) <doi:10.1198/TECH.2009.08019>. The `parallel` package is
integrated to parallelize the evaluation of the user’s design criterion.
Additionally, `GADGET` has wrapped the optimization into a sequential
routine to perform sequential computer experiments that automatically
call simulator code that is available in R.

## Installation

To install `GADGET` from github, use the `install_github` function from
the `devtools` package.

``` r
install.packages('devtools')
devtools::install_github('isaacmichaud/GADGET')
```
