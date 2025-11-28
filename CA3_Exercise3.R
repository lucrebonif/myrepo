df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))


nll_lm = function(par, data) {
  b.i = par[1:4]
  sd = par[5]
  X = cbind(1, data$x1, data$x2, data$x3)
  mu.i = X %*% b.i
  llik = dnorm(data$y, mu.i, sd, log = TRUE)
  return(-sum(llik))
} 


inits = c(mean(df$y), 0.01, 0.01, 0.01, sd(df$y))
optim(par = inits, fn = nll_lm, data = df, method="L-BFGS-B", 
      lower = c(-Inf, -Inf, -Inf, -Inf, 0.001), 
      upper = c(Inf, Inf, Inf, Inf, Inf))$par


X = cbind(1, df$x1, df$x2, df$x3)
y = df$y
beta_hat = solve(crossprod(X), crossprod(X, y))
beta_hat 


sqrt(crossprod(y - X %*% beta_hat)/28)


hess = optim(par = inits, fn = nll_lm, data = df, 
             method="L-BFGS-B", hessian = TRUE,
             lower = c(-Inf, -Inf, -Inf, -Inf, 0.001), 
             upper = c(Inf, Inf, Inf, Inf, Inf))$hessian
sqrt(diag(solve(hess)))[1:4]

lm_beta_hat = fit$coefficients
lm_sigma_hat = summary(fit)$sigma
lm_beta_hat
lm_sigma_hat
