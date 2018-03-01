
code2b <- function(theta,n){
  c1 <- (2*gamma(theta))/(2*gamma(theta)+gamma(theta+1/2))
  x <- runif(n,0,1)
  for (i in 1:n){
    if (x[i]<=c1){
      x[i] <- rgamma(1, shape = theta, scale = 1)
    }
    else{
      x[i] <- rgamma(1, shape = theta+0.5, scale = 1) 
    }
  }
  x
}

gt <- function(x,theta=5){
  1/(2*gamma(theta)+gamma(theta+0.5))*(2*x^(theta-1)+x^(theta-0.5))*exp(-x)
}

plot(density(code2b(5,10000)), ylim = c(0, 0.2), xlab = c("x"), 
     main = c("kernel density estimation and true density of g"),lty = 1,lwd = 2)
curve(gt(x,5), from = 0, to = 20, n = 10000, add = T, col="red",lty = 1,lwd = 2)
legend("topright", inset=.1, 
       legend = c("kernel density","true density"), 
       bty = "n", lty = 1, lwd = 2, col = c("black", "red"))



code2c <- function(theta,n){
  c1 <- (2*gamma(theta))/(2*gamma(theta)+gamma(theta+1/2))
  y <- rep(0,n)
  for (i in 1:n){
    process <- TRUE
    while(process){
      x <- runif(1,0,1)
      if (x<c1){
        x <- rgamma(1, shape = theta, scale = 1)
      }
      else{
        x <- rgamma(1, shape = theta+0.5, scale = 1) 
      }
      test <- runif(1,0,1) * (2*x^(theta-1)+x^(theta-1/2))*exp(-x)
      if (test<=(sqrt(4+x)*x^(theta-1)*exp(-x))){
        y[i] <- x
        process <- FALSE
      }
    }
  }
  y
}

hist(code2c(5,10000), prob=TRUE, ylim = c(0, 0.2),
     main = c("estimated density of a random sample"))



code3a <- function(theta,beta,n){
  c1 <- 1/theta/(1/theta+sqrt(3)/beta)
  y <- rep(0,n)
  for (i in 1:n){
    process <- TRUE
    while(process){
      x <- runif(1,0,1)
      if (x<=c1){
        x <- rbeta(1,theta,1)
      }
      else{
        x <- rbeta(1,1,beta) 
      }
      test <- runif(1,0,1) * (x^(theta-1)+sqrt(3)*(1-x)^(beta-1))
      if (test<=(x^(theta-1)/(1+x^2)+sqrt(2+x^2)*(1-x)^(beta-1))){
        y[i] <- x
        process <- FALSE
      }
    }
  }
  y
}

hist(code3a(5,10,10000),xlim = range(0:1), ylim = range(0:5),prob=TRUE,
     main = c("estimated density of a random sample"))

code3a <- function(theta,beta,n){
  c1 <- 1/theta/(1/theta+sqrt(3)/beta)
  y <- rep(0,n)
  for (i in 1:n){
    process <- TRUE
    while(process){
      x <- runif(1,0,1)
      if (x<=c1){
        x <- rbeta(1,theta,1)
      }
      else{
        x <- rbeta(1,1,beta) 
      }
      test <- runif(1,0,1) * (x^(theta-1)+sqrt(3)*(1-x)^(beta-1))
      if (test<=(x^(theta-1)/(1+x^2)+sqrt(2+x^2)*(1-x)^(beta-1))){
        y[i] <- x
        process <- FALSE
      }
    }
  }
  y
}

hist(code3a(5,10,10000),xlim = range(0:1), ylim = range(0:5),prob=TRUE,
     main = c("estimated density of a random sample"))



code3b <- function(theta,beta,n){
  c1 <- beta/sqrt(3)/(theta+beta/sqrt(3))
  y <- rep(0,n)
  for (i in 1:n){
    process <- TRUE
    while(process){
      x <- runif(1,0,1)
      if (x<c1){
        x <- rbeta(1,theta,1)
        test <- runif(1,0,1)*(x^(theta-1))
        if (test<=(x^(theta-1)/(1+x^2))){
          y[i] <- x
          process <- FALSE
        }
      }
      else{
        x <- rbeta(1,1,beta)
        test <- runif(1,0,1)*(sqrt(3)*(1-x)^(beta-1))
        if (test<=(sqrt(2+x^2)*(1-x)^(beta-1))){
          y[i] <- x
          process <- FALSE
        }
      }
    }
  }
  y
}


hist(code3b(3,5,10000),xlim = range(0:1), ylim = range(0:3),prob=TRUE,
     main = c("estimated density of a random sample"))




















