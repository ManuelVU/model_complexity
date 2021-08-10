# Code used to generate the main figures in the original version
# of the article, the organization of the figures is the same.

#### Figure 1: Hierarchical model example ####
load('data/unif_h.Rdata')
pdf('fig/unfi_h.pdf',height = 4)
col.fx <- "#AA596C"
par(oma=c(1,1,0.1,0.1))
par(fig=c(0,0.5,0,0.9),
    mai=c(0.5,0.5,0.1,0.1))
plot(0,0,ann=F,axes=F,type='n',ylim=c(0,10),xlim=c(0,10))
mtext("Independent",side=3,at=5,cex=1.5)
box()
axis(1,cex.axis=1.3,padj = -0.3)
axis(2,las=2,cex.axis=1.3,hadj=0.7)
for(i in 0:10){
  for(j in 0:10){
    rect(i-0.5,j-0.5,i+0.5,j+0.5,border='white',col =col.fx[26])
  }
}
par(fig=c(0.5,1,0,0.9),
    mai=c(0.5,0.5,0.1,0.1),
    new=T)
col.fx <- colorRampPalette(c("#00d4ff","#020024"))(length(unique(as.vector(unif.mu))))
cmp.col <- sort(unique(c(unif.mu)))
plot(0,0,ann=F,axes=F,type='n',ylim=c(0,10),xlim=c(0,10))
mtext("Participant 1",side=1,outer=T,at=0.53,cex=1.5,line=-0.2)
mtext(expression(paste("Hierarchical")),side=3,at=5,cex=1.5)
mtext("Participant 2",side=2,outer=T,at=0.5,cex=1.5,line=-0.5)
box()
axis(1,cex.axis=1.3,padj = -0.3)
axis(2,las=2,at=seq(0,10,2),labels = rep('',6))
for(i in 0:10){
  for(j in 0:10){
    rect(i-0.5,10-(j-0.5),i+0.5,10-(j+0.5),border='white',col = col.fx[which(cmp.col==unif.mu[i+1,j+1])])
  }
}
dev.off()

#### Figure 2: Prior predictive Psychophysical model ####
pdf(file='fig/psy_ex.pdf',height = 5.5,width = 8)
par(oma=c(3,3.5,.1,.1))
layout(rbind(c(1,1,1,1,2,2,2,2),
             c(1,1,1,1,2,2,2,2),
             c(1,1,1,1,2,2,2,2),
             c(3,3,4,4,5,5,6,6),
             c(3,3,4,4,5,5,6,6)))
par(mai=c(0.2,0.4,0.1,0.1))
par(xaxs='i',yaxs='i')
library(truncnorm)
a <- c(-100,100)
b <- c(1,100)
alpha <- rnorm(300,0,50)
beta <- rtruncnorm(300,a = 0,b = Inf, 0,100)
plot(0,0,axes=F,ann=F,xlim=c(95,905),ylim=c(-0.006,1.004))
axis(1,cex.axis=1.8)
axis(2,las=2,cex.axis=1.8,at=seq(0,1,0.2),labels=c('0',seq(0.2,0.8,0.2),'1'))

# example curves 
for(i in 1:length(alpha)){
  curve(1/(1+exp(-((x-500-alpha[i])/beta[i]))),from=100,to=900,col=paste(c(col.ssp[1],'66'),collapse=""),add=T)
}

# highlighted curves fixed alpha and fixed beta
for(i in 1:2){
  curve(1/(1+exp(-((x-500-a[i])/69))),from=100,to=900,col=col.ssp[3],add=T,lwd=i*2)
  curve(1/(1+exp(-((x-500-0)/b[i]))),from=100,to=900,col=col.ssp[4],add=T,lwd=i*2)  
}

abline(v=visual.x[2],col=col.ssp[2],lty=4,lwd=2)
box(bty='l')
mtext("Probability of long response", side=2,cex=1.4,line=3.8)
mtext("Stimulus Duration",side=1,cex=1.4,line=3)
legend('topleft',legend=c("A","B","C","D"),lwd=c(1,3,1,3),col=c(col.ssp[3],col.ssp[3],col.ssp[4],col.ssp[4]),
       bty='n',cex=1.7)

par(mai=c(0.2,0.4,0.1,0.1))

# load data from independent model simulations 
load(file = 'data/simulation_independent_psychophysics.Rdata')

tb <- table(nh.modelsim$data[which(nh.modelsim$data[,'stimulus.value']==visual.x[2]),'long.response'])
tb <- tb/sum(tb)
plot(0,0,ann=F,axes=F,type='n',xlim=c(-0.5,12.5),ylim=c(0,1))
box(bty='l')
mtext(expression(P(x)), side=2,cex=1.4,line=0.3)
mtext("Number of long responses",side=1,cex=1.4,line=3)
axis(1,seq(0,12,2),cex.axis=1.8,padj = 0)
axis(2,las=2,at=c(0,1),labels=c('0','1'),cex.axis=1.8,hadj = 0.5)
for(i in 1:length(tb)){
  rect(xleft = (i-1)-0.4,xright = (i-1)+0.4,ybottom = 0,ytop = tb[i],border=col.ssp[2],
       col=paste(c(col.ssp[2],66),collapse=''))
}
par(mai=c(0.2,0.4,0.6,0.2))
par(xaxs='r')
let <- c("A","B")
for(i in 1:2){
  theta <- 1/(1+exp(-((visual.x[2]-500-a[i])/69)))
  tb <- table(rbinom(200000,12,theta))
  tb <- tb/sum(tb)
  plot(0,0,ann=F,axes=F,type='n',xlim=c(-0.5,12.5),ylim=c(0,1))
  text(12,0.9,labels=let[i],cex=1.6)
  mtext(expression(P(x)), side=2,cex=1.2,line=0.3)
  box(bty='l')
  for(i in 1:length(tb)){
    rect(xleft = (i-1)-0.35,xright = (i-1)+0.35,ybottom = 0,ytop = tb[i],border=col.ssp[3],
         col=paste(c(col.ssp[3],66),collapse=''))
  }  
  axis(1,seq(0,12,4),cex.axis=1.8)
  axis(2,las=2,at=c(0,1),labels=c('0','1'),cex.axis=1.8,hadj = 0.6)
}
let <- c("C","D")
for(i in 1:2){
  theta <- 1/(1+exp(-((visual.x[2]-500)/b[i])))
  tb <- table(rbinom(200000,12,theta))
  tb <- tb/sum(tb)
  plot(0,0,ann=F,axes=F,type='n',xlim=c(-0.5,12.5),ylim=c(0,1))
  text(12,0.9,labels=let[i],cex=1.6)
  mtext(expression(P(x)), side=2,cex=1.2,line=0.3)
  box(bty='l')
  for(i in 1:length(tb)){
    rect(xleft = (i-1)-0.35,xright = (i-1)+0.35,ybottom = 0,ytop = tb[i],border=col.ssp[4],
         col=paste(c(col.ssp[4],66),collapse=''))
  }  
  axis(1,seq(0,12,4),cex.axis=1.8,padj = -0.2)
  axis(2,las=2,at=c(0,1),labels=c('0','1'),cex.axis=1.8,hadj = 0.6)
}
mtext("Number of long responses",side=1,cex=1.4,line=1.5,outer=T)
dev.off()

#### Figure 3 Joint Prior Predictive distribution ####

# load joint distribution hierarchical model 2 participants
load(file='data/ppd_hierarchical_psychophysics.Rdata')

# load hoint distribution independent model 2 participants
load(file='data/ppd_independent_psychophysics.Rdata')

pdf(file = 'fig/joint_ppe.pdf',width = 8,height = 3.4)
par(oma=c(2.5,3,.1,.1))
layout(matrix(seq(1,10),nrow=2))
par(mai=c(0.2,0.2,0.1,0.1))
design.points <- c(1,5,10,15,20)
for(k in design.points){
  col.fx <- colorRampPalette(c("#ce7a8d","#4d0516"))(length(unique(pp.nh[,,k])))
  col.fx.h <- colorRampPalette(c("#00d4ff","#020024"))(length(unique(pp.h[,,k])))
  cmp.col <- sort(unique(pp.nh[,,k]))
  cmp.col.h <- sort(unique(pp.h[,,k]))
  plot(0,0,ann=F,axes=F,type='n',ylim=c(0,12),xlim=c(0,12))
  box()
  axis(1,at=seq(0,12,3),labels=rep('',length(seq(0,12,3))),tck=-0.04)
  for(i in 0:12){
    for(j in 0:12){
      rect(i-0.5,j-0.5,i+0.5,j+0.5,border='white',col =col.fx[which(cmp.col==pp.nh[i+1,j+1,k])])
    }
  } 
  if(k==design.points[1]){
    mtext('Independent', side=2,line=1.6,cex=1.2)
    axis(2,at=seq(0,12,3),las=2,cex.axis=1.4,hadj=0.55,tck=-0.04)
  }
  else{
    axis(2,at=seq(0,12,3),labels=rep('',length(seq(0,12,3))),las=2,tck=-0.04)
  }
  plot(0,0,ann=F,axes=F,type='n',ylim=c(0,12),xlim=c(0,12))
  mtext(bquote(paste('Duration = ', .(visual.x[k]))),side=3,cex=1,line=0.4)
  box()
  axis(1,cex.axis=1.4,at=seq(0,12,3),padj = -0.4,tck=-0.04)
  if(k==design.points[1]){
    axis(2,at=seq(0,12,3),las=2,cex.axis=1.4,hadj=0.55,tck=-0.04)
    mtext('Hierarchical', side=2,line=1.6,cex=1.2)
  }
  else{
    axis(2,at=seq(0,12,3),labels=rep('',length(seq(0,12,3))),las=2,tck=-0.04)
  }
  for(i in 0:12){
    for(j in 0:12){
      rect(i-0.5,j-0.5,i+0.5,j+0.5,border='white',col =col.fx.h[which(cmp.col.h==pp.h[i+1,j+1,k])])
    }
  }
}
mtext('Participant 1', side=2, outer=T,line=1.6,cex=1.2)
mtext('Participant 2', side=1, outer=T,line=1.3,cex=1.2)
dev.off()

#### Figure 4 Entropy and population size ####

# colors assigned to each population size and model
col.ssp <- c("#90afc5", "#2a3132", "#4d0516","#336b87","#00d4ff")

# load entropy for multiple populations
load('data/entropy_psyphisics.Rdata')

pdf(file='fig/joint_size.pdf',width = 5,height = 3.5)
par(oma=c(2.5,3,.1,.1))
par(mai=c(0.2,0.2,0.1,0.1))
plot(1:20,ent.size[1,5,],type='l',col=col.ssp[3],lwd=1.5,ann=F,axes=F, ylim=c(0,max(ent.size[,5,])),lty=4)
lines(1:20,ent.size[1,1,],col=col.ssp[3],lwd=1.5)
points(1:20, ent.size[1,1,],bg='white',col=col.ssp[3],pch=21,cex=0.9)
points(1:20, ent.size[1,5,],bg='white',col=col.ssp[3],pch=21,cex=0.9)
lines(1:20,ent.size[2,1,],col=col.ssp[4],lwd=1.5)
lines(1:20,ent.size[2,5,],col=col.ssp[4],lwd=1.5,lty=4)
points(1:20, ent.size[2,1,],bg='white',col=col.ssp[4],pch=23,cex=0.9)
points(1:20, ent.size[2,5,],bg='white',col=col.ssp[4],pch=23,cex=0.9)
box(bty='l')
axis(1,at=c(1,5,10,15,20),labels=visual.x[c(1,5,10,15,20)])
axis(2,las=2)
legend('bottom',legend=c('two participants','six participants','hierarchical','independent'),col=c(1,1,col.ssp[c(4,3)]),
       pch = c(NA,NA,18,16),bty='n',lty=c(1,4,NA,NA),cex=0.9)
mtext("Prior Predivtive Entropy",line=2.5,side=2)
mtext("Target Stimulus Duration",line=2.3,side=1)
dev.off()
