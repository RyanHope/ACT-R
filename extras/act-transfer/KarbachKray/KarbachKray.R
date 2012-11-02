# R-file to plot data from the Karbach and Kray experiment and model

# First plot the task-switching stuff

dat <- read.table("task-switching.txt")
names(dat) <- c("condition","day","task","trialtype","trial","time")
dat <- dat[dat$trial != 1,]
dat.m <- with(dat[dat$day %in% c(1,6),],tapply(time,list(condition,day,trialtype),mean))


dat.switching <- dat.m[,,"SWITCH"] - dat.m[,,"REPEAT"]

exp.switching <- (rbind(c(379,280),c(327,146))+rbind(c(217,174),c(200,104))+rbind(c(342,267),c(305,196)))/3

#exp.switching <- rbind(c(385,280),c(325,140))
exp.switching.se <- (rbind(c(426-379,324-280),c(368-327,185-146))*sqrt(14) +
					 rbind(c(269-217,227-174),c(227-200,134-104))*sqrt(14) +
					 rbind(c(408-341,301-267),c(333-305,225-196))*sqrt(14)) /3 / sqrt(42) 
library(Hmisc)
quartz(width=7,height=5)
par(lwd=2)
errbar(2:3,exp.switching[1,],exp.switching[1,]+exp.switching.se[1,],exp.switching[1,]-exp.switching.se[1,],ylim=c(0,600),type="b",xlab="",xaxt="n",xlim=c(1,6),ylab="Switching costs (ms)")
title(main="Task Switching")
errbar(2:3,exp.switching[2,],exp.switching[2,]+exp.switching.se[2,],exp.switching[2,]-exp.switching.se[2,],type="b",pch=2,add=T)
lines(4:5,dat.switching[1,]*1000,type="b")
lines(4:5,dat.switching[2,]*1000,type="b",pch=2)
axis(1,at=2:5,labels=c("Data Pre","Data Post","Model Pre","Model Post"))
legend(1,600,legend=c("Single Task training","Task Switching training"),pch=1:2,lty=1,bty="n")

# Plot digitspan

dat <- read.table("digitspan.txt")
names(dat) <- c("day","condition","span","numresponded","numcorrect","accuracy")
dat$mult <- dat$accuracy * dat$span
dat.m <- with(dat,tapply(mult,list(condition,day),sum))
dat.m <- dat.m/length(dat$day)


preSingle <- (47.3+66.1+56.3)/3
preSwitch <- (45.8+71.6+55.1)/3
postSingle <- (47.8+68.3+58)/3
postSwitch <- (56+81.3+62.4)/3

exp.data <- rbind(c(preSingle,postSingle),c(preSwitch,postSwitch))/100
exp.data.se <- rbind(c(11.4+12.7+16.8,24+16.3+15.8),c(13.4+16.1+15.9,17.2+15.3+18.6))/300/sqrt(42)

quartz(width=7,height=5)
par(lwd=2)
errbar(2:3,exp.data[1,],exp.data[1,]+exp.data.se[1,],exp.data[1,]-exp.data.se[1,],ylim=c(0.4,1),type="b",xlab="",xaxt="n",xlim=c(1,6),ylab="Proportion correct")
title(main="Count Span")
errbar(2:3,exp.data[2,],exp.data[2,]+exp.data.se[2,],exp.data[2,]-exp.data.se[2,],type="b",pch=2,add=T)
lines(4:5,dat.m[1,],type="b")
lines(4:5,dat.m[2,],type="b",pch=2)
axis(1,at=2:5,labels=c("Data Pre","Data Post","Model Pre","Model Post"))
legend(1,1,legend=c("Single Task training","Task Switching training"),pch=1:2,lty=1,bty="n")

# Plot Stroop task

dat <- read.table("stroop.txt")
names(dat) <- c("task","condition","block","day","trial","type","correct","RT")
res <- with(dat,tapply(RT,list(day,condition,type),mean))
preSingle <- (70+30+57)/3
preSwitch <- (48+57+77)/3
postSingle <- (72+48+72)/3
postSwitch <- (24+27+56)/3

exp.stroop <- rbind(c(preSingle,postSingle),c(preSwitch,postSwitch))
exp.stroop.se <- rbind(c(42+31+57,49+41+55),c(61+41+81,53+34+46)) /3 /sqrt(42)
quartz(width=7,height=5)
par(lwd=2)
errbar(2:3,exp.stroop[1,],exp.stroop[1,]+exp.stroop.se[1,],exp.stroop[1,]-exp.stroop.se[1,],ylim=c(0,100),type="b",xlab="",xaxt="n",xlim=c(1,6),ylab="Interference (ms)")
title(main="Stroop")
errbar(2:3,exp.stroop[2,],exp.stroop[2,]+exp.stroop.se[2,],exp.stroop[2,]-exp.stroop.se[2,],type="b",pch=2,add=T)
lines(4:5,(res[,1,1]-res[,1,2])*1000,type="b")
lines(4:5,(res[,2,1]-res[,2,2])*1000,type="b",pch=2)
axis(1,at=2:5,labels=c("Data Pre","Data Post","Model Pre","Model Post"))
legend(1,100,legend=c("Single Task training","Task Switching training"),pch=1:2,lty=1,bty="n")

