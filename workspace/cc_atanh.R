### ============================================================================
###
###


pval2 = function(rho, r, N) {

  f = function(etas) {
    rhos = c(rho*etas[1]*etas[2], etas[1], etas[2])
    s = atanh(r)
    eta = atanh(rhos)
    D_inv = diag(sqrt(N - 3))
    value = sum((D_inv%*%(eta - s)^2))
    pchisq(value, 3, log.p = TRUE)
  }

  ui = rbind(c(1, 0),
             c(0, 1),
             c(-1, 0),
             c(0, -1))

  ci = c(-1, -1, -1, -1)

  optimized = constrOptim(theta = r[2:3],
                          f = f,
                          grad = NULL,
                          ui = ui,
                          ci = ci)

  1 - exp(optimized$value)

}

pval6 = function(rho, r, N) {

  D_inv = N - 3
  s = atanh(r)

  f = function(etas) {
    eta = atanh(c(rho*etas[1]*etas[2], etas[1], etas[2]))
    pchisq((D_inv %*% (eta - s)^2), df = 3, log.p = TRUE)
  }

  optimized = suppressWarnings(nlm(f = f, p = r[2:3]))

  1 - exp(optimized$minimum)

}

pval5 = function(rho, r, N) {

  D_inv = N - 3
  s = atanh(r)

  f = function(etas) {
    eta = atanh(c(rho*etas[1]*etas[2], etas[1], etas[2]))
    (D_inv %*% (eta - s)^2)
  }

  optimized = suppressWarnings(nlm(f = f,
                                   p = r[2:3]))

  1 - pchisq(optimized$minimum, df = 3)

}

pval3 = function(rho, r, N) {

  f = function(etas) {
    rhos = c(rho*etas[1]*etas[2], etas[1], etas[2])
    s = atanh(r)
    eta = atanh(rhos)
    D_inv = diag(sqrt(N - 3))
    value = sum((D_inv%*%(eta - s))^2)
    result = pchisq(value, 3, log.p = TRUE)
    gradient = dchisq(value, 3)/pchisq(value, 3)

    ## Computing the gradient
    K = 2*D_inv[1]*(eta[1] - s[1])/(1 - rhos[1]^2)
    c(etas[2]*rho)
  }

  optimized = suppressWarnings(nlm(f = f,
                                   p = r[2:3]))

  1 - exp(optimized$minimum)

}

pval4 = function(rho, r, N) {

  D_inv = sqrt(N - 3)
  s = atanh(r)

  f = function(etas) {
    eta = atanh(c(rho*etas[1]*etas[2], etas[1], etas[2]))
    (D_inv %*% (eta - s)^2)
  }

  ui = rbind(c(1, 0),
             c(0, 1),
             c(-1, 0),
             c(0, -1))

  ci = c(-1, -1, -1, -1)

  optimized = constrOptim(theta = r[2:3],
                          f = f,
                          grad = NULL,
                          ui = ui,
                          ci = ci)

  1 - pchisq(optimized$value, df = 3)

}

pval2 = function(rho, r, N) {

  D_inv = N - 3
  s = atanh(r)

  f = function(theta) {
    eta = atanh(c(rho*theta[1]*theta[2], theta[1], theta[2]))
    c(D_inv %*% (eta - s)^2)
  }

  ui = rbind(c(1, 0),
             c(0, 1),
             c(-1, 0),
             c(0, -1))

  ci = c(-1, -1, -1, -1)

  eps = 0.01
  optimized = constrOptim(theta = r[2:3]*(1 - eps),
                          f = f,
                          grad = NULL,
                          ui = ui,
                          ci = ci)

  1 - pchisq(optimized$value, df = 3)

}





xx = seq(-1, 1, by = 0.001)
yy = sapply(xx, function(rho) pval2(rho, r, N))
plot(xx, yy)



r = c(0.8, sqrt(0.5), sqrt(0.5))
N = c(100, 100, 100)

r = c(0.4, sqrt(0.4), sqrt(0.4))
N = c(1000, 1000, 1000)

rho = seq(-1, 1, by = 0.01)
x = p_value(rho, r, N)
plot(x, level = NULL)
plot(x)


p_value(0.5, r, N) -> x

r = c(-0.1, sqrt(0.1), sqrt(0.1))
N = c(1000, 1000, 10)
level = 0.95
ci(r, N, 0.95)

r = c(0.4, sqrt(0.4), sqrt(0.4))
N = c(1000, 1000, 1000)
ci(r, N, alpha)

r = c(.1, sqrt(0.3), sqrt(0.2))
N = c(1000, 1000, 1000)
ci(r, N, alpha)

r = c(-.3, sqrt(0.3), sqrt(0.2))
N = c(1000, 1000, 1000)
ci(r, N, alpha)

CI = function(r, N, alpha) {

  ## If the maximum is less than -(1 - alpha), the CI is  c(-1,1)
  maximum = optimize(f = function(rho) as.numeric(1 - p_value(rho, r, N)),
                     interval = c(-1, 1),
                     maximum = TRUE)

  if(maximum$objective < (1 - alpha)) return(c(-1, 1))

  ## If the minimum is larger than (1 - alpha), the CI is empty.
  minimum = optimize(f = function(rho) as.numeric(1 - p_value(rho, r, N)),
                     interval = c(-1, 1),
                     maximum = FALSE)

  if(minimum$objective > (1 - alpha)) return(NULL)

  f = function(rho) (p_value(rho, r, N) - alpha)^2

  # If the the right edge is greater than alpha, the CI is connected.
  if((1 - p_value(1, r, N)) > (1 - alpha)) {
    if((1 - p_value(-1, r, N)) > (1 - alpha)) {
      s = r[1]/(r[2]*r[3])
      lower = optimize(f = f,
                       interval = c(-1, s),
                       maximum = FALSE)$minimum

      upper = optimize(f = f,
                       interval = c(s, 1),
                       maximum = FALSE)$minimum
      return(c(lower, upper))
    }

    upper = optimize(f = f,
                     interval = c(-1, maximum$maximum),
                     maximum = FALSE)$minimum
    return(c(-1, upper))


  }

  # If the left edge is greater than alpha, the CI is connected. Since the
  # case of a bounded CI is covered above, it is unbounded to the right.

  if((1 - p_value(-1, r, N)) > (1 - alpha)) {
    lower = optimize(f = f,
                     interval = c(maximum$maximum, 1),
                     maximum = FALSE)$minimum
    return(c(lower, 1))
  }

  # If no edges are are greater than alpha, the CI has two components.
  lower = optimize(f = f,
                   interval = c(maximum$maximum, 1),
                   maximum = FALSE)$minimum

  upper = optimize(f = f,
                   interval = c(-1, maximum$maximum),
                   maximum = FALSE)$minimum

  list(lower = c(-1, upper),
       upper = c(lower, 1))

}





#
#
#
# etas = seq(-1, 1, by = 0.01)
#
# rho = -0.9
# f = Vectorize(function(eta1, eta2) {
#   rhos = c(rho*eta1*eta1, eta1, eta2)
#   s = atanh(r)
#   eta = atanh(rhos)
#   D_inv = diag(sqrt(N - 3))
#   value = sum((D_inv%*%(eta - s))^2)
#   value
# })
#
# vv = outer(etas, etas, f)
# contour(etas, etas, vv)

#
# microbenchmark::microbenchmark(pval(rho, r, N),
#                                pval2(rho, r, N),
#                                pval5(rho, r, N),
#                                pval6(rho, r, N))


rho = .9
microbenchmark::microbenchmark(pval(rho, r, N),
                               pval2(rho, r, N))
