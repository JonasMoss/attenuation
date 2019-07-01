#' Calculate and a confidence curve for for an attenuated correlation coefficient.
#'
#' @param r Numeric vector of three elemtents in [-1,1]. \code{r[1]} is the
#'     correlation between the noisy measures X' and Y', \code{r[2]} is the
#'     correlation between the noisy X' and the true X, while \code{r[3]} is
#'     the correlation between the noisy Y' and the true Y.
#' @param N Numeric vector of three positive integers. \code{N[i]} is the
#'     sample size for \code{r[i]}.
#' @param lower Lower bound for the curve. Defaults to -1.
#' @param upper Upper bound for the curve. Defaults to 1.
#' @param by Increment of the sequence from \code{lower} to \code{upper}.
#' @return An object of class \code{ccaf}.
#' @export

cc = function(r, N, lower = -1, upper = 1, by = 0.001) {
  rho = seq(lower, upper, by = by)
  x = 1 - p_value(rho, r, N)
  attr(x, "type") = "Confidence curve"
  x
}