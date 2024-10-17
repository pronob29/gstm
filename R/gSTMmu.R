# Optimization for Global Parameters over Doc-Topic Proportions
# Main method up top, regression implementations below.
opt.mu <- function(lambda, mode=c("CTM", "Pooled", "L1"), covar=NULL, enet=NULL, ic.k=2,
                   maxits=1000) {
  
  # CTM Mode with Gamma Noise
  if(mode=="CTM") {
    mu <- matrix(colMeans(lambda), ncol=1)
    # Add Gamma(1,1) noise term for each topic
    phi <- rgamma(ncol(mu), shape=1, rate=1)
    mu <- mu + matrix(rep(phi, each=nrow(mu)), nrow=nrow(mu), ncol=ncol(mu), byrow=TRUE)
    return(list(mu=mu, gamma=NULL))
  }
  
  # Pooled Mode with Gamma Noise
  if(mode=="Pooled") {
    gamma <- vector(mode="list", length=ncol(lambda))
    Xcorr <- crossprod(covar)
    for (i in 1:ncol(lambda)) {
      gamma[[i]] <- vb.variational.reg(Y=lambda[,i], X=covar, Xcorr=Xcorr, maxits=maxits) 
    }
    gamma <- do.call(cbind, gamma)
    mu <- t(covar %*% gamma)
    # Add Gamma(1,1) noise term for each topic
    phi <- rgamma(ncol(mu), shape=1, rate=1)
    mu <- mu + matrix(rep(phi, each=nrow(mu)), nrow=nrow(mu), ncol=ncol(mu), byrow=TRUE)
    if(!is.matrix(mu)) {
      mu <- as.matrix(mu)
    }
    return(list(mu=mu, gamma=gamma))
  }
  
  # L1 Regularized Mode (No modification needed for Gamma noise)
  if(mode=="L1") {
    out <- glmnet::glmnet(x=covar[,-1], y=lambda, family="mgaussian", alpha=enet)
    unpack <- unpack.glmnet(out, ic.k=ic.k)
    gamma <- rbind(unpack$intercept, unpack$coef)
    mu <- t(covar %*% gamma)
    if(!is.matrix(mu)) {
      mu <- as.matrix(mu)
    }
    return(list(mu=mu, gamma=gamma))
  }
}

# Variational Linear Regression with a Half-Cauchy hyperprior
# Implementation based off the various LMM examples from Matt Wand
# This code is intended to be passed a Matrix object
vb.variational.reg <- function(Y, X, b0=1, d0=1, Xcorr=NULL, maxits=1000) {
  if(is.null(Xcorr)) Xcorr <- crossprod(X)
  XYcorr <- crossprod(X, Y) 
  
  an <- (1 + nrow(X)) / 2
  D <- ncol(X)
  N <- nrow(X)
  w <- rep(0, ncol(X))
  error.prec <- 1 # Expectation of the error precision
  converge <- 1000
  cn <- ncol(X) # - 1 for the intercept and +1 in the update cancel
  dn <- 1
  Ea <- cn / dn # Expectation of the precision on the weights
  ba <- 1
  
  ct <- 1
  while(converge > .0001) {
    w.old <- w
    
    # Add the coefficient prior
    if(is.matrix(X)) {
      ppmat <- diag(x=c(0, rep(as.numeric(Ea), (D-1))), nrow=D) 
    } else {
      ppmat <- Diagonal(n=D, x=c(0, rep(as.numeric(Ea), (D-1))))
    }
    invV <- error.prec * Xcorr + ppmat
    if(is.matrix(invV)) {
      V <- chol2inv(chol(invV))
    } else {
      V <- solve(invV)     
    }
    w <- error.prec * V %*% XYcorr
    
    # Update parameters of noise model (an remains constant)
    sse <- sum((X %*% w - Y)^ 2)
    bn <- .5 * (sse + sum(diag(Xcorr %*% V))) + ba
    error.prec <- an / bn
    ba <- 1 / (error.prec + b0)
    
    # Subtract off the intercept while working out the hyperparameters
    w0 <- w[1]
    w <- w[-1]
    da <- 2 / (Ea + d0)
    dn <- 2 * da + (crossprod(w) + sum(diag(V)[-1]))
    Ea <- cn / dn
    w <- c(w0, w)
    ct <- ct + 1
    if(ct > maxits) {
      stop("Prevalence regression failing to converge within iteration limit. Try using gamma.prior='L1'.")
    }
    converge <- sum(abs(w - w.old))
  }
  return(w)
}