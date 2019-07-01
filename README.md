
<!-- README.md is generated from README.Rmd. Please edit that file -->

# attenuation <img src="man/figures/logo.png" align="right" width="177" height="65" />

An `R` package for computing confidence curves, confidence intervals and
p-values for correlation coefficients corrected for attenuation due to
measurement error.

## Installation

From inside `R`, use the following command:

``` r
# install.packages("devtools")
devtools::install_github("JonasMoss/attenuation")
```

## Usage

Use `cc` to calculate a confidence curve and `plot` to plot it.

``` r
library("attenuation")
r = c(0.20, sqrt(0.45), sqrt(0.55))
N = c(100, 100, 100)
curve = cc(r, N)
plot(curve, level = 0.90)
```

<img src="man/figures/README-simpleuse-1.png" width="750px" />
Confidence sets can be calculated with `ci`. *NB*: These sets are not
necessarily intervals. They can be either the disjoint union of two
intervals or empty.

``` r
ci(r, N)
#> [1] -0.1647174  0.9958587
```

To find the *p*-value for the nullhypothesis of a specific rho, use the
`p_value` function.

``` r
p_value(rho = 0, r, N)
#> 
#> p-value for corrected correlation coefficients
#> 
#> Arguments
#>     Sample correlations: 0.2 0.671 0.2
#>     Sample sizes: 100 100 100
#> 
#> Hypothesis test
#>     rho = 0 
#>     p-value: 0.263
```
