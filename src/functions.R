# Functions used by multiple .R files in the project. These are the main functions
# used to simulate behavior.

#### Example: Hierarchical and independent Psychophysics model #### 

# by defaul the function simulates behavior using the hierarchical model
psyphisics <- function(model='h',populations,participants,binom.samplesize,stimulus.values,standard,
                       prior.mu.a=c(0,44),prior.sigma.a=c(0,40),prior.mu.b=c(0,84),prior.sigma.b=c(0,86),
                       prior.alpha=c(0,50),prior.beta=c(0,100)){
  library(truncnorm)
  if(model=='h'){
    n.pop <- populations
    n.rep <- participants
    n.times <- binom.samplesize
    std <- standard
    dx <- sort(stimulus.values)
    mu.alpha <- rnorm(n.pop,prior.mu.a[1],prior.mu.a[2])
    sigma.alpha <- runif(n = n.pop,min=prior.sigma.a[1],max = prior.sigma.a[2])
    mu.beta <- rtruncnorm(n = n.pop,a = 0,b = Inf, mean = prior.mu.b[1], sd =prior.mu.b[2])
    sigma.beta <- runif(n = n.pop,min = prior.sigma.b[1],max = prior.sigma.b[2])
    alpha <- rnorm(n.pop*n.rep,rep(mu.alpha,each=n.rep),rep(sigma.alpha,each=n.rep))
    beta <- rtruncnorm(n = n.pop*n.rep,a = 0,b = Inf, mean = rep(mu.beta,each=n.rep), sd = rep(sigma.beta,each=n.rep))
    theta <- 1/(1+as.vector(t(exp(-(t(outer(dx-std,alpha,FUN = '-'))*1/beta)))))
    alpha.ret <- rep(alpha,each=length(dx))
    beta.ret <- rep(beta,each=length(dx))
    dx.ret <- rep(dx,times=n.pop*n.rep)
    ch <- sapply(theta, rbinom,n=1,size=n.times)
    pop <- rep(seq(1,n.pop),each=length(dx)*n.rep)
    part <- rep(rep(seq(1,n.rep),each=length(dx)),times=n.pop)
    nn <- rep(seq(1,n.pop*n.rep),each=length(dx))
    output <- list()
    output$data <- cbind(nn,part,pop,ch,dx.ret,theta,alpha.ret,beta.ret)
    colnames(output$data) <- c('partnumber','participant','population','long.response','stimulus.value','probability.long','alpha','beta')
    output$simdata <- data.frame('model'='Hierarchical',
                                 'n.populations'=n.pop,
                                 'n.participants'=n.rep,'binomial.samplesize'=n.times,
                                 'standard.stimulus' = std,
                                 'stimulus'=dx,
                                 'hyperparameters'=list(alpha=list(mu=list(mean=prior.mu.a[1],sd=prior.mu.a[2]),
                                                                   sigma=list(mean=prior.sigma.a[1],sd=prior.sigma.a[2])),
                                                        beta=list(mu=list(mean=prior.mu.a[1],sd=prior.mu.a[2]),
                                                                  sigma=list(mean=prior.sigma.b[1],sd=prior.sigma.b[2]))))
  }
  if(model=='n-h'){
    n.pop <- populations
    n.rep <- participants
    n.times <- binom.samplesize
    std <- standard
    dx <- sort(stimulus.values)
    alpha <- rnorm(n.pop*n.rep,prior.alpha[1],prior.alpha[2])
    beta <- rtruncnorm(n = n.pop*n.rep,a = 0,b = Inf, mean = prior.beta[1], sd = prior.beta[2])
    theta <- 1/(1+as.vector(t(exp(-(t(outer(dx-std,alpha,FUN = '-'))*1/beta)))))
    alpha.ret <- rep(alpha,each=length(dx))
    beta.ret <- rep(beta,each=length(dx))
    dx.ret <- rep(dx,times=n.pop*n.rep)
    ch <- sapply(theta, rbinom,n=1,size=n.times)
    pop <- rep(seq(1,n.pop),each=length(dx)*n.rep)
    part <- rep(rep(seq(1,n.rep),each=length(dx)),times=n.pop)
    nn <- rep(seq(1,n.pop*n.rep),each=length(dx))
    output <- list()
    output$data <- cbind(nn,part,pop,ch,dx.ret,theta,alpha.ret,beta.ret)
    colnames(output$data) <- c('partnumber','participant','population','long.response','stimulus.value','probability.long','alpha','beta')
    output$simdata <- data.frame('n.populations'=n.pop,
                                 'n.participants'=n.rep,
                                 'binomial.samplesize'=n.times,
                                 'standard.stimulus' = std,
                                 'stimulus'=dx,
                                 'parameters'=list(alpha=list(mean=prior.alpha[1],sd=prior.alpha[2]),
                                                   beta=list(mean=prior.beta[1],sd=prior.beta[2])))
  }
  return(output)
}
