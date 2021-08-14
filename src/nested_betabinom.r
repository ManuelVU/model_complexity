# Example for nested models using the beta-binomial model

#### Prior predictive distribution and entropy surface for n=10 ####
# sample size for simulations
n <- 10

# possible outcomes
x <- seq(0,10)

# prior predictive as an array with 3 dimentions, alpha, beta
# and and support.
prior.pred <- array(NA,dim=c(100,100,length(x)))

# entropy surface as a matrix, alpha values in rows and beta in columns 
entropy <- matrix(NA,ncol=100,nrow=100)
for(i in 1:100){
  for(j in 1:100){
    for(k in 1:length(x)){
      prior.pred[i,j,k] <- (factorial(n)/(factorial(x[k])*factorial(n-x[k])))*(beta((x[k]+i),(n-x[k]+j))/beta(i,j))
    }
    entropy[i,j] <- -sum(prior.pred[i,j,]*log(prior.pred[i,j,]))
  }
}

# save prior predictive distribution in nested_ppd.Rdata file
save(prior.pred,file='data/nested_ppd.Rdata')

# save entropy surface in nested_entropysurface.Rdata file
save(entropy,file='data/nested_entropysurface.Rdata')

#### Corssingprior predictive entropy

# sample sizes for simulations
siz <- seq(1,30)

# vector entropy for fixed parameter model
e.binom <- c()

# array for entropy of prior predictive distribution with 
# different sample sizes rows for sample sizes and columns for
# hyperparameters
e.beta <- matrix(NA,ncol=2,nrow=length(siz))

# prior values of alpha and beta 
hyp <- rbind(c(6,6),c(10,2))

for(p in 1:2){
  for(i in 1:length(siz)){
    e.binom[i] <- -sum(dbinom(seq(1,siz[i]),siz[i],0.5)*dbinom(seq(1,siz[i]),siz[i],0.5,log=T))
    x <- seq(1,siz[i])
    prior.pred <- c()
    for(k in 1:length(x)){
      prior.pred[k] <- (factorial(siz[i])/(factorial(x[k])*factorial(siz[i]-x[k])))*(beta((x[k]+hyp[p,1]),(siz[i]-x[k]+hyp[p,2]))/beta(hyp[p,1],hyp[p,2]))
    }
    e.beta[i,p] <- -sum(prior.pred*log(prior.pred))
  }
}

# Save entropy with multiple sample sizes and prior values of alpha and beta
save(e.beta,file = 'data/nested_beta_entropysamplesize.Rdata')

# Save entropy of fixed parameter model for multiple sample sizes
save(e.binom,file = 'data/nested_binom_entropysamplesize.Rdata')