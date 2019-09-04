# Functions needed to compute exponential smoothing forecasts.
iterate <- function(theta, y, e_0, l_0, d_0, w_0, do.forecast = FALSE){
  phi   <- theta[1]
  alpha <- theta[2]
  delta <- theta[3]
  omega <- theta[4]
  
  n <- length(y)
  yhat <- e <- l <- d <- w <- numeric(n) + NA
  if(do.forecast){
    H <- 48
    stopifnot(H <= 48)
    nf <- n + H   
    ysim <-  yhat <- e <- l <- d <- w <- numeric(nf) + NA
  }
  
  
  yhat_local_0 <- e_local_0 <- l_local_0 <- d_local_0 <- w_local_0 <- z <- numeric(m_2 * 2) + NA
  e_local_0[seq(1, m_2)] <- e_0
  l_local_0[seq(1, m_2)] <- l_0
  d_local_0[seq(1, m_2)] <- d_0
  w_local_0[seq(1, m_2)] <- w_0
  z[seq(m_2 + 1, 2 * m_2)] <- y[seq(m_2)]
  
  for(i in seq(m_2 + 1, 2 * m_2)){
    
    yhat_local_0[i]   <- l_local_0[i - 1] + d_local_0[i - m_1] + w_local_0[i - m_2] + phi * e_local_0[i - 1]
    e_local_0[i] <- z[i] - (l_local_0[i - 1] + d_local_0[i - m_1] + w_local_0[i - m_2]) # added - m_2 for y
    l_local_0[i] <- l_local_0[i - 1] + alpha * e_local_0[i]
    d_local_0[i] <- d_local_0[i - m_1] + delta * e_local_0[i]
    w_local_0[i] <- w_local_0[i - m_2] + omega * e_local_0[i]
  }
  yhat[seq(m_2)]   <- yhat_local_0[m_2 + seq(1, m_2)]
  e[seq(m_2)] <- e_local_0[m_2 + seq(1, m_2)]
  l[seq(m_2)] <- l_local_0[m_2 + seq(1, m_2)]
  d[seq(m_2)] <- d_local_0[m_2 + seq(1, m_2)]
  w[seq(m_2)] <- w_local_0[m_2 + seq(1, m_2)]
  
  for(i in seq(m_2 + 1, n)){
    yhat[i]   <- l[i - 1] + d[i - m_1] + w[i - m_2] + phi * e[i - 1]
    e[i] <- y[i] - (l[i - 1] + d[i - m_1] + w[i - m_2])
    l[i] <- l[i - 1] + alpha * e[i]
    d[i] <- d[i - m_1] + delta * e[i]
    w[i] <- w[i - m_2] + omega * e[i]
  }
  
  # mean forecast
  if(do.forecast){
    
    for(h in seq(H)){
      i <- n 
      stopifnot(h <= m_1)
      # yhat[i + h] <- l[i] + (alpha * phi * (1 - phi^(h - 1))) / ((1 - phi) * e[i])  + d[i - m_1 + h] + w[i - m_2 + h] + phi^h * e[i]
      yhat[i + h] <- l[i] + (alpha * phi * (1 - phi^(h - 1)) / (1 - phi)) * e[i]  + d[i - m_1 + h] + w[i - m_2 + h] + phi^h * e[i]
    }
    mf <- yhat[n + seq(H)]
  }
  
  # sample paths + quantile forecast
  if(do.forecast){
    K <- 5000
    samples <- matrix(NA, nrow = K, ncol = H)
    
    residuals <- y - head(yhat, -H)
    mat_residuals <- matrix(residuals, ncol = 48, byrow = T)
    # bootstrap vectors of size 48 from the residuals
    ind <- sample(seq(nrow(mat_residuals)), K, replace = TRUE)
    
    for(k in seq(K)){
      varepsilon <- mat_residuals[ind[k], ]
      for(h in seq(H)){
        i <- n + h
        ysim[i]   <- l[i - 1] + d[i - m_1] + w[i - m_2] + phi * e[i - 1] + varepsilon[h]
        e[i] <- ysim[i] - (l[i - 1] + d[i - m_1] + w[i - m_2])
        l[i] <- l[i - 1] + alpha * e[i]
        d[i] <- d[i - m_1] + delta * e[i]
        w[i] <- w[i - m_2] + omega * e[i]
      }
      samples[k, ] <- ysim[n + seq(H)]
    }
    qf <- apply(samples, 2, quantile, prob = taus)
    # matplot(t(qf), type = 'l', lty = 1)
    mfsample <- apply(samples, 2, mean)
    
    yhat <- head(yhat, -H)
    e    <- head(e, -H)
    l    <- head(l, -H)
    d    <- head(d, -H) 
    w    <- head(w, -H)
  }
  
  if(do.forecast){
    RET <- list(yhat = yhat, e = e, l = l, d = d, w = w, mf = mf, qf = qf, residuals = residuals, mfsample = mfsample)
  }else{
    RET <- list(yhat = yhat)
  }
  RET
}

func_to_optimize <- function(theta, y, e_0, l_0, d_0, w_0, do.forecast){
  obj <- iterate(theta, y, e_0, l_0, d_0, w_0, do.forecast)
  mean((y - obj$yhat)^2)
}

