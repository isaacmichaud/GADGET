
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GADGET: Gaussian Process Approximations for Designed Experiments

The `GADGET` package computes near-optimal Bayesian experimental designs
using Gaussian process optimization. At its core is the ability to
calculate static designs that maximize a design criterion that may be
either deterministic or stochastic. In particular, stochastic design
criteria could be a Monte Carlo estimator of an expected utility based
on MCMC posterior draws. `GADGET` utilizes the algorithm proposed by
Weaver et al. (2016) and performs Gaussian process validation using the
statistics introduced by Bastos and O’Hagan (2009). The `parallel`
package is integrated to parallelize the evaluation of the user’s design
criterion. Additionally, `GADGET` has wrapped the optimization into a
sequential routine to perform sequential computer experiments that
automatically call simulator code that is available in R.

## Installation

To install `GADGET` from github, use the `install_github` function from
the `devtools` package.

``` r
install.packages('devtools')
devtools::install_github('isaacmichaud/GADGET')
```

## References

Bastos, L. S., & O’Hagan, A. (2009). Diagnostics for gaussian process
emulators. Technometrics, 51(4), 425–438.

Weaver, B. P., Williams, B. J., Anderson-Cook, C. M., Higdon, D. M.
(2016). Computational enhancements to Bayesian design of experiments
using Gaussian processes. Bayesian Analysis, 11(1), 191–213.
