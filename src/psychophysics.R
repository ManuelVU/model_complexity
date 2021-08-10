# This code is used for the analysis of the Psychophisics model
# in the hierarchical models section.

#### Loading functions and data ####

# truncated normal distribution functions
library(truncnorm)

# loading data from the original experiment
driel <- R.matlab::readMat('data/vanDrielData2015.mat')

# loding functions for simulation
source('src/functions.R')

#### Extracting calues of the experimental design ####

# extract values of the visual stimuli on the experiment
visual.st <- driel$d[[6]]

# order unique values in the experiment from lowest to highest
visual.x <- sort(unique(visual.st[1,]))

#### Array to save prior predictive entropies ####

# array has 3 dimentions, first one is models (1=independent,
# 2 = hierarchical). Second is poulation size (1 = 2 participants,
# 2 = 3 participants, 3 = 4 participants, 4 = 5 participants,
# 5 = 6 participants). Third is number of stimulus, each 
# simulation uses the 20 stimulus in the experimental task. 

ent.size <- array(NA,dim=c(2,5,20))

#### Simulation: population of 2 participants ####

# 2 participants using hierarchical model, using default prior
# distributions in the function.
h.modelsim <- psyphisics(populations = 300000,participants = 2,
                         binom.samplesize = 12,
                         stimulus.values = visual.x,
                         standard = 500)

# 2 participants using the independent model, using default
# prior distributions in the function.
nh.modelsim <- psyphisics(model = 'n-h',populations = 300000,
                          participants = 2,binom.samplesize = 12,
                          stimulus.values = visual.x,
                          standard = 500)

# number of participants in the simulation
n.p <- 2

# array for hoint count of sucssess by stimulus value for the 
# hierarchical model, dimentions are: 
# 1. 13 possible results (0 to 12)
# 2. number of participants in a population
# 3. number of stimulus in the experiment 
pp.h <- array(rep(0,prod(c(rep(13,n.p),20))),dim=c(rep(13,n.p),length(visual.x)))

# array for the joint count of success by stimulus value for the 
# independent model, dimentions are the same.
pp.nh <- array(rep(0,prod(c(rep(13,n.p),20))),dim=c(rep(13,n.p),length(visual.x)))

# count hierarchical model
for(e in 1:length(visual.x)){
  datos <- c()
  for(j in 1:n.p){
    datos <- cbind(datos,
                   h.modelsim$data[which(h.modelsim$data[,'stimulus.value']==visual.x[e]&h.modelsim$data[,'participant']==j),'long.response'])
  }
  for(i in 1:length(datos[,1])){
    pp.h[(datos[i,1]+1),(datos[i,2]+1),e] <- 
      pp.h[(datos[i,1]+1),(datos[i,2]+1),e]+1
  }
}

# count independent model
for(e in 1:length(visual.x)){
  datos <- c()
  for(j in 1:n.p){
    datos <- cbind(datos,nh.modelsim$data[which(nh.modelsim$data[,'stimulus.value']==visual.x[e]&nh.modelsim$data[,'participant']==j),'long.response'])
  }
  for(i in 1:length(datos[,1])){
    pp.nh[(datos[i,1]+1),(datos[i,2]+1),e] <- pp.nh[(datos[i,1]+1),(datos[i,2]+1),e]+1
  }
}

# estimated prior predictive entropy by stimulus value
ent <- c()
ent.nh <- c()
for(e in 1:length(visual.x)){
  ent[e] <- -sum((c(pp.h[,,e])[pp.h[,,e]>0]/sum(pp.h[,,e]))*log((c(pp.h[,,e])[pp.h[,,e]>0]/sum(pp.h[,,e]))))
  ent.nh[e] <- -sum(c(pp.nh[,,e])[pp.nh[,,e]>0]/sum(pp.nh[,,e])*log((c(pp.nh[,,e])[pp.nh[,,e]>0]/sum(pp.nh[,,e]))))
}

# add entropies of independent model to main array
ent.size[1,1,] <- ent.nh

# add entropies of hierarchical model to main array
ent.size[2,1,] <- ent

# save prior predictive distribution independent model 
# for example plot
save(nh.modelsim,file = 'data/ppd_independent_psychophysics.Rdata')

# remove simulations and counts (needed if RAM is less than 32 gb)
rm(list=c('h.modelsim','nh.modelsim','pp.h','pp.nh','ent', 'ent.nh')) 

#### Simulation: population of 3 participants ####

# 3 participants using hierarchical model, using default prior
# distributions in the function.
h.modelsim <- psyphisics(populations = 300000,participants = 3,
                         binom.samplesize = 12,
                         stimulus.values = visual.x,
                         standard = 500)

# 3 participants using the independent model, using default
# prior distributions in the function.
nh.modelsim <- psyphisics(model = 'n-h',populations = 300000,
                          participants = 3,binom.samplesize = 12,
                          stimulus.values = visual.x,
                          standard = 500)

# number of participants in the simulation
n.p <- 3

# array for hoint count of sucssess by stimulus value for the 
# hierarchical model, dimentions are: 
# 1. 13 possible results (0 to 12)
# 2. number of participants in a population
# 3. number of stimulus in the experiment 
pp.h <- array(rep(0,prod(c(rep(13,n.p),20))),dim=c(rep(13,n.p),length(visual.x)))

# array for the joint count of success by stimulus value for the 
# independent model, dimentions are the same.
pp.nh <- array(rep(0,prod(c(rep(13,n.p),20))),dim=c(rep(13,n.p),length(visual.x)))

# count hierarchical model
for(e in 1:length(visual.x)){
  datos <- c()
  for(j in 1:n.p){
    datos <- cbind(datos,
                   h.modelsim$data[which(h.modelsim$data[,'stimulus.value']==visual.x[e]&h.modelsim$data[,'participant']==j),'long.response'])
  }
  for(i in 1:length(datos[,1])){
    pp.h[(datos[i,1]+1),(datos[i,2]+1),(datos[i,3]+1),e] <- pp.h[(datos[i,1]+1),(datos[i,2]+1),(datos[i,3]+1),e]+1
  }
}

# count independent model
for(e in 1:length(visual.x)){
  datos <- c()
  for(j in 1:n.p){
    datos <- cbind(datos,nh.modelsim$data[which(nh.modelsim$data[,'stimulus.value']==visual.x[e]&nh.modelsim$data[,'participant']==j),'long.response'])
  }
  for(i in 1:length(datos[,1])){
    pp.nh[(datos[i,1]+1),(datos[i,2]+1),(datos[i,3]+1),e] <- pp.nh[(datos[i,1]+1),(datos[i,2]+1),(datos[i,3]+1),e]+1
  }
}

# estimated prior predictive entropy by stimulus value
ent <- c()
ent.nh <- c()
for(e in 1:length(visual.x)){
  ent[e] <- -sum((c(pp.h[,,,e])[pp.h[,,,e]>0]/sum(pp.h[,,,e]))*log((c(pp.h[,,,e])[pp.h[,,,e]>0]/sum(pp.h[,,,e]))))
  ent.nh[e] <- -sum(c(pp.nh[,,,e])[pp.nh[,,,e]>0]/sum(pp.nh[,,,e])*log((c(pp.nh[,,,e])[pp.nh[,,,e]>0]/sum(pp.nh[,,,e]))))
}

# add entropies of independent model to main array
ent.size[1,2,] <- ent.nh

# add entropies of hierarchical model to main array
ent.size[2,2,] <- ent

# remove simulations and counts (needed if RAM is less than 32 gb)
rm(list=c('h.modelsim','nh.modelsim','pp.h','pp.nh','ent', 'ent.nh'))

#### Simulation: population of 4 participants ####

# 4 participants using hierarchical model, using default prior
# distributions in the function.
h.modelsim <- psyphisics(populations = 300000,participants = 4,
                         binom.samplesize = 12,
                         stimulus.values = visual.x,
                         standard = 500)

# 4 participants using the independent model, using default
# prior distributions in the function.
nh.modelsim <- psyphisics(model = 'n-h',populations = 300000,
                          participants = 4,binom.samplesize = 12,
                          stimulus.values = visual.x,
                          standard = 500)

# number of participants in the simulation
n.p <- 4

# array for hoint count of sucssess by stimulus value for the 
# hierarchical model, dimentions are: 
# 1. 13 possible results (0 to 12)
# 2. number of participants in a population
# 3. number of stimulus in the experiment 
pp.h <- array(rep(0,prod(c(rep(13,n.p),20))),dim=c(rep(13,n.p),length(visual.x)))

# array for the joint count of success by stimulus value for the 
# independent model, dimentions are the same.
pp.nh <- array(rep(0,prod(c(rep(13,n.p),20))),dim=c(rep(13,n.p),length(visual.x)))

# count hierarchical model
for(e in 1:length(visual.x)){
  datos <- c()
  for(j in 1:n.p){
    datos <- cbind(datos,
                   h.modelsim$data[which(h.modelsim$data[,'stimulus.value']==visual.x[e]&h.modelsim$data[,'participant']==j),'long.response'])
  }
  for(i in 1:length(datos[,1])){
    pp.h[(datos[i,1]+1),(datos[i,2]+1),(datos[i,3]+1),(datos[i,4]+1),e] <- pp.h[(datos[i,1]+1),(datos[i,2]+1),(datos[i,3]+1),(datos[i,4]+1),e]+1
  }
}

# count independent model
for(e in 1:length(visual.x)){
  datos <- c()
  for(j in 1:n.p){
    datos <- cbind(datos,nh.modelsim$data[which(nh.modelsim$data[,'stimulus.value']==visual.x[e]&nh.modelsim$data[,'participant']==j),'long.response'])
  }
  for(i in 1:length(datos[,1])){
    pp.nh[(datos[i,1]+1),(datos[i,2]+1),(datos[i,3]+1),(datos[i,4]+1),e] <- pp.nh[(datos[i,1]+1),(datos[i,2]+1),(datos[i,3]+1),(datos[i,4]+1),e]+1
  }
}

# estimated prior predictive entropy by stimulus value
ent <- c()
ent.nh <- c()
for(e in 1:length(visual.x)){
  ent[e] <- -sum((c(pp.h[,,,,e])[pp.h[,,,,e]>0]/sum(pp.h[,,,,e]))*log((c(pp.h[,,,,e])[pp.h[,,,,e]>0]/sum(pp.h[,,,,e]))))
  ent.nh[e] <- -sum(c(pp.nh[,,,,e])[pp.nh[,,,,e]>0]/sum(pp.nh[,,,,e])*log((c(pp.nh[,,,,e])[pp.nh[,,,,e]>0]/sum(pp.nh[,,,,e]))))
}

# add entropies of independent model to main array
ent.size[1,3,] <- ent.nh

# add entropies of hierarchical model to main array
ent.size[2,3,] <- ent

# remove simulations and counts (needed if RAM is less than 32 gb)
rm(list=c('h.modelsim','nh.modelsim','pp.h','pp.nh','ent', 'ent.nh'))

#### Simulation: population of 5 participants ####

# 5 participants using hierarchical model, using default prior
# distributions in the function.
h.modelsim <- psyphisics(populations = 500000,participants = 5,
                         binom.samplesize = 12,
                         stimulus.values = visual.x,
                         standard = 500)

# 5 participants using the independent model, using default
# prior distributions in the function.
nh.modelsim <- psyphisics(model = 'n-h',populations = 500000,
                          participants = 5,binom.samplesize = 12,
                          stimulus.values = visual.x,
                          standard = 500)

# number of participants in the simulation
n.p <- 5

# array for hoint count of sucssess by stimulus value for the 
# hierarchical model, dimentions are: 
# 1. 13 possible results (0 to 12)
# 2. number of participants in a population
# 3. number of stimulus in the experiment 
pp.h <- array(rep(0,prod(c(rep(13,n.p),20))),dim=c(rep(13,n.p),length(visual.x)))

# array for the joint count of success by stimulus value for the 
# independent model, dimentions are the same.
pp.nh <- array(rep(0,prod(c(rep(13,n.p),20))),dim=c(rep(13,n.p),length(visual.x)))

# count hierarchical model
for(e in 1:length(visual.x)){
  datos <- c()
  for(j in 1:n.p){
    datos <- cbind(datos,
                   h.modelsim$data[which(h.modelsim$data[,'stimulus.value']==visual.x[e]&h.modelsim$data[,'participant']==j),'long.response'])
  }
  for(i in 1:length(datos[,1])){
    pp.h[(datos[i,1]+1),(datos[i,2]+1),(datos[i,3]+1),(datos[i,4]+1),(datos[i,5]+1),e] <- 
      pp.h[(datos[i,1]+1),(datos[i,2]+1),(datos[i,3]+1),(datos[i,4]+1),(datos[i,5]+1),e]+1
  }
}

# count independent model
for(e in 1:length(visual.x)){
  datos <- c()
  for(j in 1:n.p){
    datos <- cbind(datos,nh.modelsim$data[which(nh.modelsim$data[,'stimulus.value']==visual.x[e]&nh.modelsim$data[,'participant']==j),'long.response'])
  }
  for(i in 1:length(datos[,1])){
    pp.nh[(datos[i,1]+1),(datos[i,2]+1),(datos[i,3]+1),(datos[i,4]+1),(datos[i,5]+1),e] <- 
      pp.nh[(datos[i,1]+1),(datos[i,2]+1),(datos[i,3]+1),(datos[i,4]+1),(datos[i,5]+1),e]+1
  }
}

# estimated prior predictive entropy by stimulus value
ent <- c()
ent.nh <- c()
for(e in 1:length(visual.x)){
  ent[e] <- -sum((c(pp.h[,,,,,e])[pp.h[,,,,,e]>0]/sum(pp.h[,,,,,e]))*log((c(pp.h[,,,,,e])[pp.h[,,,,,e]>0]/sum(pp.h[,,,,,e]))))
  ent.nh[e] <- -sum(c(pp.nh[,,,,,e])[pp.nh[,,,,,e]>0]/sum(pp.nh[,,,,,e])*log((c(pp.nh[,,,,,e])[pp.nh[,,,,,e]>0]/sum(pp.nh[,,,,,e]))))
}

# add entropies of independent model to main array
ent.size[1,4,] <- ent.nh

# add entropies of hierarchical model to main array
ent.size[2,4,] <- ent

# remove simulations and counts (needed if RAM is less than 32 gb)
rm(list=c('h.modelsim','nh.modelsim','pp.h','pp.nh','ent', 'ent.nh'))

#### Simulation: population of 6 participants ####

# 6 participants using hierarchical model, using default prior
# distributions in the function.
h.modelsim <- psyphisics(populations = 750000,participants = 6,
                         binom.samplesize = 12,
                         stimulus.values = visual.x,
                         standard = 500)

# 5 participants using the independent model, using default
# prior distributions in the function.
nh.modelsim <- psyphisics(model = 'n-h',populations = 750000,
                          participants = 6,binom.samplesize = 12,
                          stimulus.values = visual.x,
                          standard = 500)

# number of participants in the simulation
n.p <- 6

# array for hoint count of sucssess by stimulus value for the 
# hierarchical model, dimentions are: 
# 1. 13 possible results (0 to 12)
# 2. number of participants in a population
# 3. number of stimulus in the experiment 
pp.h <- array(rep(0,prod(c(rep(13,n.p),20))),dim=c(rep(13,n.p),length(visual.x)))

# array for the joint count of success by stimulus value for the 
# independent model, dimentions are the same.
pp.nh <- array(rep(0,prod(c(rep(13,n.p),20))),dim=c(rep(13,n.p),length(visual.x)))

# count hierarchical model
for(e in 1:length(visual.x)){
  datos <- c()
  for(j in 1:n.p){
    datos <- cbind(datos,
                   h.modelsim$data[which(h.modelsim$data[,'stimulus.value']==visual.x[e]&h.modelsim$data[,'participant']==j),'long.response'])
  }
  for(i in 1:length(datos[,1])){
    pp.h[(datos[i,1]+1),(datos[i,2]+1),(datos[i,3]+1),(datos[i,4]+1),(datos[i,5]+1),(datos[i,6]+1),e] <- 
      pp.h[(datos[i,1]+1),(datos[i,2]+1),(datos[i,3]+1),(datos[i,4]+1),(datos[i,5]+1),(datos[i,6]+1),e]+1
  }
}

# count independent model
for(e in 1:length(visual.x)){
  datos <- c()
  for(j in 1:n.p){
    datos <- cbind(datos,nh.modelsim$data[which(nh.modelsim$data[,'stimulus.value']==visual.x[e]&nh.modelsim$data[,'participant']==j),'long.response'])
  }
  for(i in 1:length(datos[,1])){
    pp.nh[(datos[i,1]+1),(datos[i,2]+1),(datos[i,3]+1),(datos[i,4]+1),(datos[i,5]+1),(datos[i,6]+1),e] <- 
      pp.nh[(datos[i,1]+1),(datos[i,2]+1),(datos[i,3]+1),(datos[i,4]+1),(datos[i,5]+1),(datos[i,6]+1),e]+1
  }
}

# estimated prior predictive entropy by stimulus value
ent <- c()
ent.nh <- c()
for(e in 1:length(visual.x)){
  ent[e] <- -sum((c(pp.h[,,,,,,e])[pp.h[,,,,,,e]>0]/sum(pp.h[,,,,,,e]))*log((c(pp.h[,,,,,,e])[pp.h[,,,,,,e]>0]/sum(pp.h[,,,,,,e]))))
  ent.nh[e] <- -sum(c(pp.nh[,,,,,,e])[pp.nh[,,,,,,e]>0]/sum(pp.nh[,,,,,,e])*log((c(pp.nh[,,,,,,e])[pp.nh[,,,,,,e]>0]/sum(pp.nh[,,,,,,e]))))
}

# add entropies of independent model to main array
ent.size[1,5,] <- ent.nh

# add entropies of hierarchical model to main array
ent.size[2,5,] <- ent

#### Save array with entropies ####
save(ent.size,file='data/entropy_psyphisics.Rdata')