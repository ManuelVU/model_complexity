# Code use for the simulation and analysis of the example using 
# Luce's Choice rule.

#### Simulation of general and nested model with four experimental designs ####

# simulation, design 1 Coricelli, et. al. (2005)
source('src/functions.R')
nsim <-100000
repetitions <- 20
design <- exp_des()
d1_one <- prior_predictive(gamble = design,trials = repetitions,n.samples = nsim,model='one')
e1_one <- entropy_choice(d1_one[,1],unique(d1_one[,'p']),model='one',trials=repetitions)
d1_gamma <- prior_predictive(gamble = design,trials = repetitions,n.samples = nsim)
e1_gamma <- entropy_choice(d1_gamma,unique(d1_one[,'id']),trials=repetitions)

# save prior predictive distribution and prior predictive entropy
# for plot example
save()