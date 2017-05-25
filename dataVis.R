rm(list=ls(all=TRUE))
library(fields)

# function to convert levels to numeric or characters
LtoN <- function(x) as.numeric(as.character(x))
LtoC <- function(x) as.character(x)

output=read.csv("output.csv")

nb=length(unique(output$path))
cat=unique(output$category)
states=unique(output$state)

sum=matrix(NA,length(cat),5,dimnames=list(LtoC(cat),c("obs","frequency","lakes","year","depths")))

for(i in cat){
  matTemp=output[output$category==i,]
  sum[i,"frequency"]=nrow(matTemp)/nb
  sum[i,"year"]=median(matTemp$nbYears)
  sum[i,"depths"]=mean(matTemp$nbDepths)
  sum[i,"lakes"]=sum(matTemp$nbLakes)
  sum[i,"obs"]=sum(matTemp$nbObs)
}
ppi=300
png("falk_dist.png",width=5*ppi, height=6*ppi,bg="transparent",res=ppi)
hist(output[output$category=="alkalinity","nbLakes"])
dev.off()

heatMat=matrix(NA,length(cat),length(states),dimnames=list(cat,states))

cat
states
j="OH"
i="hardness"
for(i in cat){
  for(j in states){
   lgt=length(unique(output[output$state==j,"path"]))
    var=nrow((output[output$category==i&output$state==j,]))
    heatMat[i,j]=var/lgt
  }
}


ppi=300
png("freq_year.png",width=7*ppi, height=6*ppi,bg="transparent",res=ppi)
#mybar=barplot(sum[,"frequency"]/nb,ann=FALSE,width=0.8, xaxt="n")
mybar=barplot(sum[,"frequency"]/nb,ann=FALSE,width=0.8, xaxt="n")
text(cex=1, x=seq(0.7,7.7,1), y=-0.02, cat, xpd=TRUE, srt=45, pos=2)
xlim0 <- par()$usr[1:2]
par(new = TRUE)
plot.new()
#plot.window(xlim = xlim0, ylim = c(0, 600000), xaxs = "i")
#points(sum[,"obs"] ~ mybar,pch=16,lwd=2, col = "darkred",type="o")
plot.window(xlim = xlim0, ylim = c(0, 25), xaxs = "i")
points(sum[,"year"] ~ mybar,pch=16,lwd=2, col = "darkred",type="o")
axis(side = 4, col = "darkred")
dev.off()

  ppi=300
  png("heatMat.png",width=7*ppi, height=6*ppi,bg="transparent",res=ppi)
  image.plot(heatMat,col=rev(heat.colors(50)))
  dev.off()
