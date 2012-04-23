# Load all the Draco data
D <- read.table('All Draco.txt',header=TRUE)

maximus <- D[(D$model == 'Draco' | D$model == 'Draco2') & (D$v==7 | D$v == 5.6),]
melanopogon <- D[D$model=='Draco' & D$v==3,]
allflat <- rbind(maximus,melanopogon)
superdraco <- D[D$model=='Draco' & D$v==12,]
camber <- D[(D$model == 'Camber'),]


quartz('Draco midglide and initial lift coefficients',width=3,height=2.5,pointsize=8,file="CL vs AOA.pdf")
par(mar=c(4,4,1,1),mgp=c(2.5,1,0))
plot(maximus$aoa,maximus$Cl,type="n",xlab='Angle of attack, degrees',ylab='Lift coefficient',las=1,ylim=c(-0.2,1.1))
abline(c(0,0),col='grey')
points(maximus$aoa,maximus$Cl,pch=16,col='red')
points(melanopogon$aoa,melanopogon$Cl,col='red')
#points(superdraco$aoa,superdraco$Cl,pch=5)
points(camber$aoa,camber$Cl,pch=18,col='blue')
smaoa <- seq(-15,90,1)
camber.smL <- predict(loess(Cl~aoa,data=camber), data.frame(aoa=smaoa))
lines(smaoa,camber.smL,col="blue")
allflat.smL <-predict(loess(Cl~aoa,data=allflat),data.frame(aoa=smaoa))
lines(smaoa,allflat.smL,col="red")



quartz('Draco midglide and initial drag coefficients',width=3,height=2.5,pointsize=8,file="CD vs AOA.pdf")
par(mar=c(4,4,1,1),mgp=c(2.5,1,0))
plot(maximus$aoa,maximus$Cd,type="n",xlab='Angle of attack, degrees',ylab='Drag coefficient',las=1,ylim=c(0,1.3))
abline(c(0,0),col='grey')
points(maximus$aoa,maximus$Cd,pch=16,col='red')
points(melanopogon$aoa,melanopogon$Cd,col='red')
#points(superdraco$aoa,superdraco$Cl,pch=5)
points(camber$aoa,camber$Cd,pch=18,col='blue')
camber.smD <- predict(loess(Cd~aoa,data=camber), data.frame(aoa=smaoa))
lines(smaoa,camber.smD,col="blue")
allflat.smD <-predict(loess(Cd~aoa,data=allflat),data.frame(aoa=smaoa))
lines(smaoa,allflat.smD,col="red")


quartz('Draco midglide and initial pitching moment coefficients',width=3,height=2.5,pointsize=8,file="Cm vs AOA.pdf")
par(mar=c(4,4,1,1),mgp=c(2.5,1,0))
plot(maximus$aoa,-maximus$Cm,type="n",xlab='Angle of attack, degrees',ylab='Pitching moment coefficient',las=0,ylim=c(-0.07,0.07))
abline(c(0,0),col='grey')
points(maximus$aoa,-maximus$Cm,pch=16,col='red')
points(melanopogon$aoa,-melanopogon$Cm,col='red')
#points(superdraco$aoa,superdraco$Cl,pch=5)
points(camber$aoa,-camber$Cm,pch=18,col='blue')
camber.smm <- predict(loess(-Cm~aoa,data=camber), data.frame(aoa=smaoa))
lines(smaoa,camber.smm,col="blue")
allflat.smm <-predict(loess(-Cm~aoa,data=allflat),data.frame(aoa=smaoa))
lines(smaoa,allflat.smm,col="red")

