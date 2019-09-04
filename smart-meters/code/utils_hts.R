# Functions needed to compute MinT forecasts.

shrink.estim <- function(x, tar)
{
  if (is.matrix(x) == TRUE && is.numeric(x) == FALSE)
    stop("The data matrix must be numeric!")
  p <- ncol(x)
  n <- nrow(x)
  
  covm <- crossprod(x) / n
  corm <- cov2cor(covm)
  xs <- scale(x, center = FALSE, scale = sqrt(diag(covm)))
  v <- (1/(n * (n - 1))) * (crossprod(xs^2) - 1/n * (crossprod(xs))^2)
  diag(v) <- 0
  corapn <- cov2cor(tar)
  d <- (corm - corapn)^2
  lambda <- sum(v)/sum(d)
  lambda <- max(min(lambda, 1), 0)
  shrink.cov <- lambda * tar + (1 - lambda) * covm
  return(list(shrink.cov = shrink.cov, lambda = lambda ))
}

vec_w <- function(x){
  n <- nrow(x)
  apply(x, 2, crossprod) / n
}
lowerD <- function(x)
{
  n <- nrow(x)
  return(diag(vec_w(x)))
}
