#' Calculate a confidence interval for an attenuated correlation coefficient.
#'
#' @param r Numeric vector of three elemtents in [-1,1]. \code{r[1]} is the
#'     correlation between the noisy measures X' and Y', \code{r[2]} is the
#'     correlation between the noisy X' and the true X, while \code{r[3]} is
#'     the correlation between the noisy Y' and the true Y.
#' @param N Numeric vector of three positive integers. \code{N[i]} is the
#'     sample size for \code{r[i]}.
#' @param level Numeric in [0, 1]. Confidence level of the interval. Defaults to
#'     0.95.
#' @param positive Logical; If \code{TRUE}, the correlations \code{r[2]} and
#'     \code{r[3]} are forced to be positive.
#' @return Numeric in [0, 1]. The p-value under null-hypothesis rho.
#' @examples
#'     r = c(0.20, sqrt(0.45), sqrt(0.55))
#'     N = c(100, 100, 100)
#'     ci(r, N) # Calculates 95% confidence set for rho.
#' @export

ci = function(r, N, level = 0.95, positive = FALSE) {

  alpha = 1 - level

  ## If the maximum is less than -(1 - alpha), the CI is  c(-1,1)
  maximum = stats::optimize(f = function(rho) as.numeric(1 - p_value(rho, r, N, positive = positive)),
                     interval = c(-1, 1),
                     maximum = TRUE)

  if(maximum$objective < (1 - alpha)) return(c(-1, 1))

  ## If the minimum is larger than (1 - alpha), the CI is empty.
  minimum = stats::optimize(f = function(rho) as.numeric(1 - p_value(rho, r, N, positive = positive)),
                     interval = c(-1, 1),
                     maximum = FALSE)

  if(minimum$objective > (1 - alpha)) return(NULL)

  f = function(rho) (p_value(rho, r, N, positive = positive) - alpha)^2

  # If the the right edge is greater than alpha, the CI is connected.
  if((1 - p_value(1, r, N, positive = positive)) > (1 - alpha)) {
    if((1 - p_value(-1, r, N, positive = positive)) > (1 - alpha)) {
      s = r[1]/(r[2]*r[3])
      lower = stats::optimize(f = f,
                       interval = c(-1, s),
                       maximum = FALSE)$minimum

      upper = stats::optimize(f = f,
                       interval = c(s, 1),
                       maximum = FALSE)$minimum
      return(c(lower, upper))
    }

    upper = stats::optimize(f = f,
                     interval = c(-1, maximum$maximum),
                     maximum = FALSE)$minimum
    return(c(-1, upper))


  }

  # If the left edge is greater than alpha, the CI is connected. Since the
  # case of a bounded CI is covered above, it is unbounded to the right.

  if((1 - p_value(-1, r, N, positive = positive)) > (1 - alpha)) {
    lower = stats::optimize(f = f,
                     interval = c(maximum$maximum, 1),
                     maximum = FALSE)$minimum
    return(c(lower, 1))
  }

  # If no edges are are greater than alpha, the CI has two components.
  lower = stats::optimize(f = f,
                   interval = c(maximum$maximum, 1),
                   maximum = FALSE)$minimum

  upper = stats::optimize(f = f,
                   interval = c(-1, maximum$maximum),
                   maximum = FALSE)$minimum

  list(lower = c(-1, upper),
       upper = c(lower, 1))

}

