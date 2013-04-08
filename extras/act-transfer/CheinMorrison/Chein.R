# R-File to plot the data from the Chein and Morrison experiment


### This plots the WMC data against the model fit

dada <- c( 0, 5.9, 6.7, 0.8, 3.6, 9.2, 6.6, 13.9, 11.2, 13.9, 11.8, 9.1, 12.5, 12.8, 12.8, 16.8, 15.5, 13.9, 20.8, 22.5)
dada <- (dada/100+1)*4.33 
dat <- read.table("WMChein.txt")
names(dat) <- c("day","span","correct")
dat.m <- with(dat[dat$correct==1,],tapply(span,day,mean))
#dat.m <- with(dat,tapply(span,day,mean))
quartz(width=5,height=5)
par(lwd=2)
plot(dat.m,type="b",ylab="WM Span (items)",xlab="Training session",ylim=c(1,7))
lines(dada,type="b",pch=2)
legend(1,6,legend=c("Data","Model"),pch=c(2,1),lty=1)


# This plots the Stroop data and model fit

dat <- read.table("stroopChein.txt")
names(dat) <- c("task","condition","block","day","trial","type","correct","RT")
res <- with(dat[dat$block>0,],tapply(RT,list(day,condition,type),mean))

exp.stroop <- rbind(c(120,95),c(120,60))

quartz(width=7,height=5)
par(lwd=2)
plot(2:3,exp.stroop[1,],ylim=c(0,160),type="b",xlab="",xaxt="n",xlim=c(1,6),ylab="Interference (ms)",main="Stroop")
lines(2:3,exp.stroop[2,],type="b",pch=2)
lines(4:5,(res[,1,2]-res[,1,1])*1000,type="b")
lines(4:5,(res[,2,2]-res[,2,1])*1000,type="b",pch=2)
axis(1,at=2:5,labels=c("Data Pre","Data Post","Model Pre","Model Post"))
legend(1,160,legend=c("No training","WM training"),pch=1:2,lty=1)

dat$subject <- as.factor(as.integer((as.numeric(rownames(dat))-1)/768))
res <- with(dat,tapply(RT,list(day,condition,type,subject),mean))
res[,,2,]-res[,,1,]

# This plots just the WMC data (as it is in the paper)

dada <- c( 0, 5.9, 6.7, 0.8, 3.6, 9.2, 6.6, 13.9, 11.2, 13.9, 11.8, 9.1, 12.5, 12.8, 12.8, 16.8, 15.5, 13.9, 20.8, 22.5)
dada <- (dada/100+1)*4.33 
quartz(width=5,height=5)
par(lwd=2)
plot(dada,type="b",ylab="WM Span (items)",xlab="Training session",ylim=c(3,6))
legend(1,6,legend=c("Data","Model"),pch=c(2,1),lty=1)



# This plots just the Stroop data (as it is in the paper)
library(Hmisc)
exp.stroop <- rbind(c(120.3,91.5),c(127.8,58.7))
exp.stroop.se <- rbind(c(135.9,103.1),c(148.0,78.8))-exp.stroop
quartz(width=4,height=5)
par(lwd=2)
errbar(2:3,exp.stroop[1,],exp.stroop[1,]+exp.stroop.se[1,],exp.stroop[1,]-exp.stroop.se[1,],ylim=c(0,160),type="b",xlab="",xaxt="n",xlim=c(1,4),ylab="Stroop Interference (ms)")
errbar(2:3,exp.stroop[2,],exp.stroop[2,]+exp.stroop.se[2,],exp.stroop[2,]-exp.stroop.se[2,],add=T,type="b",pch=2)
axis(1,at=2:3,labels=c("Data Pre","Data Post"))
legend(1,25,legend=c("No training","WM training"),pch=1:2,lty=1,bty="n")



# This plots the rehearsal against the non-rehearsal model

# WMC comparison
dat2 <- read.table("WMCheinNR.txt")
dat <- read.table("WMChein.txt")
names(dat) <- c("day","span","correct")
names(dat2) <- c("day","span","correct")
dat.m <- with(dat[dat$correct==1,],tapply(span,day,mean))
dat2.m <- with(dat2[dat2$correct==1,],tapply(span,day,mean))

#dat.m <- with(dat,tapply(span,day,mean))
quartz(width=5,height=5)
par(lwd=2)
plot(dat.m,type="b",ylab="WM Span (items)",xlab="Training session",ylim=c(1,7))
lines(dat2.m,type="b",pch=2)
legend(1,7,legend=c("Reactive model","Proactive model"),pch=c(2,1),lty=1,bty="n")


# Stroop comparison

dat <- read.table("stroopChein.txt")
dat2 <- read.table("stroopCheinNR.txt")

names(dat) <- c("task","condition","block","day","trial","type","correct","RT")
names(dat2) <- c("task","condition","block","day","trial","type","correct","RT")
res <- with(dat,tapply(RT,list(day,condition,type),mean))
res2 <- with(dat2,tapply(RT,list(day,condition,type),mean))


quartz(width=7,height=5)
par(lwd=2)
plot(2:3,(res2[,1,2]-res2[,1,1])*1000,ylim=c(0,200),type="b",xlab="",xaxt="n",xlim=c(1,6),ylab="Interference (ms)")
lines(2:3,(res2[,2,2]-res2[,2,1])*1000,type="b",pch=2)
lines(4:5,(res[,1,2]-res[,1,1])*1000,type="b")
lines(4:5,(res[,2,2]-res[,2,1])*1000,type="b",pch=2)
axis(1,at=2:5,labels=c("Pre","Post","Pre","Post"))
text(c(2.5,5),c(115,90),labels=c("Reactive\nmodel","Proactive\nmodel"))
legend(1,50,legend=c("No training","WM training"),pch=1:2,lty=1,bty="n")



