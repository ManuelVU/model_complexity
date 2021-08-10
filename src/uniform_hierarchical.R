# This code is used for the analysis of the Uniform model
# in the hierarchical models section.

# truncated normal functions
library(truncnorm)

# sample size for binomial distribution
repetitions <- 10

# probability of success p1 independent
ta <- runif(300000,0,1)

# probability of success p2 independent
tb <- runif(300000,0,1)

# mean probability of success for group
mu <- runif(150000,0,1)

# probability of success for pairs of participants hierarchical model 
theta <- t(sapply(mu,rtruncnorm,n=2,sd=0.001,a=0,b=1))

# binomial random variable for independen model
y <- cbind(sapply(ta,rbinom,size=repetitions,n=1),sapply(tb,rbinom,size=repetitions,n=1))

# binomial random variable for hierarchical model
x <- cbind(sapply(theta[,1],rbinom,size=repetitions,n=1),sapply(theta[,2],rbinom,size=repetitions,n=1))

# matrix with the joint count of successes independent model
unif <- matrix(NA,ncol=repetitions+1,nrow=repetitions+1)

# matrix with the joint count of successes hierarchical model
unif.mu <- matrix(NA,ncol=repetitions+1,nrow=repetitions+1)

# count results of joint
for(i in 0:(repetitions)){
  for(j in 0:(repetitions)){
    unif[(11-i),(j+1)] <- sum(y[,1]==i&y[,2]==j)
    unif.mu[(11-i),(j+1)] <- sum(x[,1]==i&x[,2]==j)
  }
}

# matrix with the joint probability mass of successes independent model
px <- unif/sum(unif)

# matrix with the joint probability mass of successes hierarchical model
px.h <- unif.mu/sum(unif.mu)

# save joint posterior counts for hierarchical model
save(unif.mu, file = 'data/unif_h.Rdata')