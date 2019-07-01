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
#' @return Numeric in [0, 1]. The p-value under null-hypothesis rho.
#' @export

p_value = function(rho, r, N) {

  fun = function(rho) {

    D_inv = N - 3
    s = atanh(r)

    fn = function(theta) {
      eta = atanh(c(rho*theta[1]*theta[2], theta[1], theta[2]))
      c(D_inv %*% (eta - s)^2)
    }

    eps = 0.01
    optimized = suppressWarnings(stats::optim(par = r[2:3]*(1 - eps),
                                       fn = fn))

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
