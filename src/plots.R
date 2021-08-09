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