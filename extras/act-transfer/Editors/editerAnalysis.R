# R file to make graphs of the model results of the editor model
# And also to plot the data

# Read in the model results

dat <- read.table("res-ed2.txt")
names(dat) <- c("condition","day","editor","trial","type","ll","mt","time")
dat.m <- with(dat,tapply(time,list(condition,day),mean))



# This is the aggregate data from the experiment

dat.ededemacs <- c(115, 54, 44, 42, 43, 28)
dat.edtedtemacs <- c(214, 87, 55, 49, 43, 28)
dat.ededtemacs <- c(115,54,63,44,41,26)
dat.edtedemacs <- c(214, 87, 46, 37, 41, 26)
dat.emacsemacsemacs <- c(77, 37, 29, 23, 23, 21)


# Plot the data

quartz(width=6,height=6)
#par(mfrow=c(2,1))
par(lwd=2)
plot(1:6,dat.ededemacs,ylim=c(0,100),xlab="Day",ylab="Seconds/correct operation",type="b",pch=2)
abline(v=2.5,lty=3,lwd=1)
abline(v=4.5,lty=3,lwd=1)

lines(dat.edtedtemacs,type="b",pch=4)
lines(dat.ededtemacs,type="b",pch=4,lty=2)
lines(dat.edtedemacs,type="b",pch=2,lty=2)
lines(dat.emacsemacsemacs,type="b",pch=1)
legend(3.5,90,legend=c("edt-edt-emacs","ed-ed-emacs","edt-ed-emacs","ed-edt-emacs","emacs-emacs-emacs"),lty=c(1,1,3,3,1),pch=c(4,2,2,4,1),bg="white")

# Plot the model

quartz(width=6,height=6)
par(lwd=2)
plot(1:6,dat.m["ED-ED-EMACS",],xlab="Day",ylab="Seconds/correct operation",type="b",pch=2,ylim=c(0,100))
lines(dat.m["EDT-EDT-EMACS",],type="b",pch=4)
lines(dat.m["ED-EDT-EMACS",],type="b",pch=4,lty=2)
lines(dat.m["EDT-ED-EMACS",],type="b",pch=2,lty=2)
lines(dat.m["EMACS-EMACS-EMACS",],type="b")
legend(3.5,90,legend=c("edt-edt-emacs","ed-ed-emacs","edt-ed-emacs","ed-edt-emacs","emacs-emacs-emacs"),lty=c(1,1,3,3,1),pch=c(4,2,2,4,1),bg="white")

# Calculate transfer

# Global transfer
transfer <- function(M1,Mn,Tn) { (M1-Tn)/(M1-Mn) }
dat.ed2edt <- transfer(214,55,63)
dat.edt2ed <- transfer(115,44,46)
dat.line2emacs <- transfer(77,23,(43+43+41+41)/4)

model.ed2edt <- transfer(dat.m["EDT-EDT-EMACS",1],dat.m["EDT-EDT-EMACS",3],dat.m["ED-EDT-EMACS",3])
model.edt2ed <- transfer(dat.m["ED-ED-EMACS",1],dat.m["ED-ED-EMACS",3],dat.m["EDT-ED-EMACS",3])
model.line2emacs <- transfer(dat.m["EMACS-EMACS-EMACS",1],dat.m["EMACS-EMACS-EMACS",5],(dat.m["ED-ED-EMACS",5]+dat.m["EDT-ED-EMACS",5]+dat.m["ED-EDT-EMACS",5]+dat.m["EDT-EDT-EMACS",5])/4)

# Transfer split in LL phase and MT phase

dat.m <- with(dat,tapply(ll,list(condition,day),mean))
#model.ed2edt <- 
transfer(dat.m["EDT-EDT-EMACS",1],dat.m["EDT-EDT-EMACS",3],dat.m["ED-EDT-EMACS",3])
#model.edt2ed <- 
transfer(dat.m["ED-ED-EMACS",1],dat.m["ED-ED-EMACS",3],dat.m["EDT-ED-EMACS",3])
#model.line2emacs <- 
transfer(dat.m["EMACS-EMACS-EMACS",1],dat.m["EMACS-EMACS-EMACS",5],(dat.m["ED-ED-EMACS",5]+dat.m["EDT-ED-EMACS",5]+dat.m["ED-EDT-EMACS",5]+dat.m["EDT-EDT-EMACS",5])/4)

dat.m <- with(dat,tapply(mt,list(condition,day),mean))
#model.ed2edt <- 
transfer(dat.m["EDT-EDT-EMACS",1],dat.m["EDT-EDT-EMACS",3],dat.m["ED-EDT-EMACS",3])
#model.edt2ed <- 
transfer(dat.m["ED-ED-EMACS",1],dat.m["ED-ED-EMACS",3],dat.m["EDT-ED-EMACS",3])
#model.line2emacs <- 
transfer(dat.m["EMACS-EMACS-EMACS",1],dat.m["EMACS-EMACS-EMACS",5],(dat.m["ED-ED-EMACS",5]+dat.m["EDT-ED-EMACS",5]+dat.m["ED-EDT-EMACS",5]+dat.m["EDT-EDT-EMACS",5])/4)

