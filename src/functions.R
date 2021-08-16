# Functions used by multiple .R files in the project. These are the main functions
# used to simulate behavior.

#### Example: Hierarchical and independent Psychophysics model #### 

# By defaul the function simulates behavior using the hierarchical model
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

#### Example: Nested models, Luce's choice model ####

# This function generates the values for the experimental designs 
# used in the examples, by default, the function generated the 
# values for the stimulus in Coricelli, et. al. 2005.
exp_des <- function(author='cor'){
  if(author=='cor'){
    v <- c(-200,-50,50,200)
    vl <- combn(v,2)
    p <- c(0.2,0.5,0.8)
    loteries <- matrix(NA,nrow=18,ncol=5)
    ii <- 0
    for(i in 1:length(vl[1,])){
      for(j in 1:length(p)){
        ii <- ii+1
        loteries[ii,1] <- vl[1,i]
        loteries[ii,2] <- p[j]
        loteries[ii,3] <- vl[2,i]
        loteries[ii,4] <- 1-p[j]
        loteries[ii,5] <- vl[1,i]*p[j]+(1-p[j])*vl[2,i]
      }
    }
    design <- {}
    x <- c()
    for(i in 1:(length(loteries[,1])-1)){
      for(j in (i+1):length(loteries[,1])){
        if(loteries[i,5]!=loteries[j,5]){
          x <- c(loteries[i,5],loteries[j,5])
        }
        design <- rbind(design,x)
      }
    }
    design <- design[which(design[,1]>0&design[,2]>0),]
  }
  else if(author=='akt'){
    h <- c(20,15)
    l <- c(0,5)
    p <- seq(0,1,0.05)
    gambles <- cbind(rep(h[1],length(p)),p,rep(l[1],length(p)),(1-p),
                     rep(h[2],length(p)),p,rep(l[2],length(p)),(1-p))
    design <- {}
    design <- cbind((gambles[,1]*gambles[,2])+(gambles[,3]*gambles[,4]),
                    (gambles[,5]*gambles[,6])+(gambles[,7]*gambles[,8]))
  }
  else if(author=='tve'){
    tev_69_set1 <- rev(seq(4,5,0.25))
    p_set1 <- seq(7,11)/24
    tev_69_set2 <- rev(seq(4,5,0.25))
    p_set2 <- seq(8,16,2)/24
    tev_69_set3 <- rev(seq(3.3,3.7,0.1))
    p_set3 <- seq(7,11,1)/24
    
    d1_69 <- cbind(tev_69_set1*p_set1,c((tev_69_set1*p_set1)[-1],(tev_69_set1*p_set1)[1]))
    d2_69 <- cbind(tev_69_set2*p_set2,c((tev_69_set2*p_set2)[-1],(tev_69_set2*p_set2)[1]))
    d3_69 <- cbind(tev_69_set3*p_set3,c((tev_69_set1*p_set3)[-1],(tev_69_set1*p_set3)[1]))
    
    design <-array(NA, dim=c(15,2))
    design <-rbind(d1_69,d2_69,d3_69)
  }
  else if (author=='hs19'){
    design <- rbind(c(5.00 ,6.95),c(0.55 ,2.50 ),c(15.00, 20.85),c(1.65 ,7.50 ),c(59.50, 61.25),c(49.00, 50.75),c(49.00, 52.50),c(50.75, 52.50),c(33.25, 35.00),c(28.00, 35.00),
                    c(28.00, 33.25),c(7.00 ,8.75 ),c(63.00, 63.00),c(35.00, 35.00),c(35.00, 35.00),c(35.00, 35.00),c(21.00, 21.00),c(21.00, 21.00),c(21.00, 21.00),
                    c(7.00 ,7.00 ),c(63.00, 61.25),c(42.00, 36.75),c(42.00, 35.00),c(36.75, 35.00),c(21.00, 19.25),c(21.00, 17.50),c(19.25, 17.50),c(10.50, 8.75),
                    c(63.00, 59.50),c(42.00, 35.00),c(42.00, 31.50),c(35.00, 31.50),c(28.00, 24.50),c(28.00, 21.00),c(24.50, 21.00),c(14.00, 10.50),c(63.00, 56.00),
                    c(52.50, 42.00),c(52.50, 35.00),c(42.00, 35.00),c(28.00, 21.00),c(31.50, 21.00),c(31.50, 28.00),c(21.00, 14.00),c(63.00, 63.00),c(35.00, 35.00),
                    c(35.00, 35.00),c(35.00, 35.00),c(21.00, 21.00),c(21.00, 21.00),c(21.00, 21.00),c(7.00  ,7.00 ),c(63.00, 56.00),c(52.50, 42.00),c(52.50, 35.00),
                    c(42.00, 35.00),c(28.00, 21.00),c(31.50, 21.00),c(31.50, 28.00),c(21.00, 14.00),c(59.50, 51.80),c(17.50, 13.30),c(17.50, 21.00),c(13.30, 21.00),
                    c(3.50 ,18.90),c(3.50 ,26.60),c(18.90, 26.60),c(24.50, 32.20),c(59.50, 33.60),c(43.75, 4.90),c(43.75, 21.00),c(4.90 ,21.00),c(0.70 ,26.60),
                    c(12.25, 26.60),c(12.25, 0.70),c(3.50 ,29.40),c(52.50, 50.00),c(45.00, 42.50),c(45.00, 40.00),c(42.50, 40.00),c(22.50, 20.00),c(30.00, 20.00),
                    c(30.00, 22.50),c(15.00, 12.50),c(50.00, 47.50),c(35.00, 27.50),c(35.00, 25.00),c(27.50, 25.00),c(20.00, 17.50),c(20.00, 15.00),c(17.50, 15.00),
                    c(12.50, 10.00),c(42.00, 40.50),c(33.00, 30.00),c(33.00, 28.50),c(30.00, 28.50),c(27.00, 25.50),c(27.00, 24.00),c(25.50, 24.00),c(21.00, 19.50))
  }
  return(design)
}

# This function simulates the behavior of a participant using 
# Luce's choice rule for a given experimental design and sample
# size. By default, the simulation is generated usgin the gamma 
# general model with prior parameters alpha=2, beta=1. A shift 
# in the distribution can be added. 
prior_predictive <- function(gamble, trials=10, model='gamma', n.samples=10000, prior_parameters=c(),shift=0){
  if(model=='gamma'){
    if(is.null(prior_parameters)){
      prior_parameters <- c(2,1)
    }
    else{
      prior_parameters <- prior_parameters
    }
    pred <- array(NA,dim=c(length(gamble[,1])*n.samples,6))
    pp <- {}
    lambda <- rgamma(n.samples,prior_parameters[1],prior_parameters[2])+shift
    theta_gamma <- outer(gamble[,1],lambda,"^")/(outer(gamble[,1],lambda,"^")+outer(gamble[,2],lambda,"^"))
    pp <- cbind(rep(gamble[,1],n.samples),rep(gamble[,2],n.samples),as.vector(t(sapply(as.vector(theta_gamma), rbinom,n=1,size=trials))),rep(trials,length(gamble[,1])*n.samples),as.vector(theta_gamma),lambda,seq(1,length(gamble[,1])))
    pred <- pp
  }
  else if(model=='one'){
    pred <- array(NA,dim=c(length(gamble[,1])*n.samples,6))
    theta_gamma <- gamble[,1]/(gamble[,1]+gamble[,2])
    pp <- cbind(rep(gamble[,1],n.samples),rep(gamble[,2],n.samples),as.vector(t(sapply(theta_gamma, rbinom,n=n.samples,size=trials))),rep(trials,length(gamble[,1])*n.samples),rep(theta_gamma,n.samples),rep(1,length(gamble[,1])*n.samples),seq(1,length(gamble[,1])))
    pred <- pp
  }
  colnames(pred) <- c('gamble a','gamble b','binom','n','p','lambda','id')
  return(pred)
}

# This function returns the prior predictive mass for a given 
# value of i, it is used to integrate over the parameter space of
# the general model. 
p.theta <- function(theta,a,b,y,trials,hyper){
  return(dbinom(y,trials,theta)*dgamma(log((1-theta)/theta)/log(b/a),hyper[1],hyper[2])*abs((-1/(theta*(1-theta)))/(log(b/a))))
}

# This function returns the prior predictive mass for a given 
# value of i, it is used to integrate over the parameter space of
# the general model with a shift parameter.
p.phi <- function(lambda,a,b,y,trials,shift){
  return(dbinom(y,trials,1/(1+((a/b)^(-lambda))))*dgamma(lambda-shift,2,1))
}

# This function returns the prior predictive entropy of Luce's 
# choice model in the general, shifted general and fixed parameter
# form.
prior.inference.choice <- function(support,va,vb,fixed.lambda=FALSE,change=FALSE,sphi,prior=c(2,1)){
  e <- c()
  p <- c()
  count <- 0
  if(fixed.lambda==F&change==F){
    for(i in support){
      count <- count +1
      p[count] <- integrate(p.theta,lower=0,upper=1,a=va,b=vb,y=i,trials=max(support),hyper=prior)$value
      e[count] <- p[count]*log(p[count])
    }
  }
  else if(fixed.lambda==F&change==T){
    for(i in support){
      count <- count +1
      p[count] <- integrate(p.phi,lower=sphi,upper=Inf,a=va,b=vb,y=i,trials=max(support),shift=sphi)$value
      e[count] <- p[count]*log(p[count])
    }
  }
  else{
    for(i in support){
      count <- count+1
      p[count] <- dbinom(i,max(support),va/(va+vb))
      e[count] <- p[count]*log(p[count])
    }
  }
  result <- list()
  result$entropy <- -sum(e)
  result$priorpred <- p   
  return(result)
}