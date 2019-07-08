#' Calculate the p-value for an attenuated correlation coefficient.
#'
#' @param rho Numeric vector in [-1,1]. The correlation under the null
#'     hypothesis.
#' @param r Numeric vector of three elemtents in [-1,1]. \code{r[1]} is the
#'     correlation between the noisy measures X' and Y', \code{r[2]} is the
#'     correlation between the noisy X' and the true X, while \code{r[3]} is
#'     the correlation between the noisy Y' and the true Y.
#' @param N Numeric vector of three positive integers. \code{N[i]} is the
#'     sample size for \code{r[i]}.
#' @param positive Logical; If \code{TRUE}, the correlations \code{r[2]} and
#'     \code{r[3]} are forced to be positive.
#' @return Numeric in [0, 1]. The p-value under the null-hypothesis that the
#'     true correlation is rho.
#' @examples
#'     r = c(0.20, sqrt(0.45), sqrt(0.55))
#'     N = c(100, 100, 100)
#'     p_value(rho = 0, r, N) # Tests rho = 0.
#' @export

p_value = function(rho, r, N, positive = FALSE) {

  fun = function(rho) {

    D_inv = N - 3
    s = atanh(r)

    fn = function(theta) {
      eta = atanh(c(rho*theta[1]*theta[2], theta[1], theta[2]))
      c(D_inv %*% (eta - s)^2)
    }

    eps = 0.01

    if(positive) {
      ui = diag(2)
      ci = rep(0, 2)
      optimized = suppressWarnings(stats::constrOptim(theta = abs(r[2:3])*(1 - eps),
                                                      f = fn,
                                                      grad = NULL,
                                                      ui = ui,
                                                      ci = ci))
    } else {
      optimized = suppressWarnings(stats::optim(par = r[2:3]*(1 - eps),
                                                fn = fn))
    }

    value = 1 - stats::pchisq(optimized$value, df = 3)

  }

  values = sapply(rho, fun)

  class(values) = c("ccaf")
  attr(values, "rho") = rho
  attr(values, "N") = N
  attr(values, "r") = r
  attr(values, "type") = "p-value"
  values

}

#' Calculate the p-value for an attenuated correlation coefficient with Cronbach's alpha
#'
#' @param rho Numeric vector in [-1,1]. The correlation under the null
#'     hypothesis.
#' @param r Numeric vector of three elemtents in [-1,1]. \code{r[1]} is the
#'     correlation between the noisy measures X' and Y', \code{r[2]} is the
#'     reliability between the noisy X' and the true X, while \code{r[3]} is
#'     the reliability between the noisy Y' and the true Y.
#' @param N Numeric vector of three positive integers. \code{N[i]} is the
#'     sample size for \code{r[i]}.
#' @param k Numeric vector of two positive integers. \code{k[i]} is the number
#'     of testlets for the for \code{r[i+1]}.
#' @return Numeric in [0, 1]. The p-value under the null-hypothesis that the
#'     true correlation is rho.
#' @export

p_value_alpha = function(rho, r, N, k) {

  fun = function(rho) {

    D_inv = c(N[1] - 3, 2*N[2:3]*(k-1)/k)
    s = c(atanh(r[1]),
          0.5*log(1 - r[2]),
          0.5*log(1 - r[3]))

    fn = function(theta) {
      eta = c(atanh(rho*sqrt(theta[1])*sqrt(theta[2])),
              0.5*log(1 - theta[1]),
              0.5*log(1 - theta[2]))
      c(D_inv %*% (eta - s)^2)
    }

    eps = 0.001
    optimized = suppressWarnings(optim(par = abs(r[2:3])*(1 - eps),
                                                 fn = fn))

    value = 1 - stats::pchisq(optimized$value, df = 3)

#
#     D_inv = c(N[1] - 3, 2*N[2:3]*(k-1)/k)
#     s = c(atanh(r[1]),
#           0.5*log(1 - r[2]),
#           0.5*log(1 - r[3]))
#
#     fn = function(theta) {
#       eta = c(atanh(rho*theta[1]*theta[2]),
#               0.5*log(1 - theta[1]^2),
#               0.5*log(1 - theta[2]^2))
#       c(D_inv %*% (eta - s)^2)
#     }
#
#     eps = 0.001
#     ui = rbind(-diag(2), diag(2))
#
#     ci = c(-rep(1, 2), rep(0, 2))
#     optimized = (stats::constrOptim(theta = c(0.01, 0.01),
#                                                     f = fn,
#                                                     grad = NULL,
#                                                     ui = ui,
#                                                     ci = ci))
#
#     value = 1 - stats::pchisq(optimized$value, df = 3)

  }

  values = sapply(rho, fun)

  class(values) = c("ccaf")
  attr(values, "rho") = rho
  attr(values, "N") = N
  attr(values, "r") = r
  attr(values, "type") = "p-value"
  values

}

