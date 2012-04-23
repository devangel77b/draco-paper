# Load all the Draco data
D <- read.table('All Draco.txt',header=TRUE)

# Get LD, etc... 
D$LD <- D$L/D$D
# Note there's screwy shit going on in NoWing data.  Best to redo. 
NoWing <- D[D$model=='NoWing' & D$Re > 40000 & D$date.run != "20090414B" & D$date.run != "20090513A" & D$aoa != 20,]
HalfWing <- D[D$model=='HalfWing' & D$Re > 40000 & D$aoa != 20,]
Draco <- D[(D$model=='Draco' | D$model == 'Draco2') & D$Re > 40000,]
DoubleWing <- D[D$model=='DoubleWing' & D$Re > 40000,]

# Do some fits too
aoa.fit <- seq(-15,90,1)
predCl <- predict(loess(Cl~aoa,data=NoWing),data.frame(aoa=aoa.fit))
predCd <- predict(loess(Cd~aoa,data=NoWing),data.frame(aoa=aoa.fit))
predCm <- predict(loess(Cm~aoa,data=NoWing),data.frame(aoa=aoa.fit))
NoWing.pred <- data.frame(aoa=aoa.fit,Cl=predCl,Cd=predCd,Cm=predCm,LD=predCl/predCd)

predCl <- predict(loess(Cl~aoa,data=Draco),data.frame(aoa=aoa.fit))
predCd <- predict(loess(Cd~aoa,data=Draco),data.frame(aoa=aoa.fit))
predCm <- predict(loess(Cm~aoa,data=Draco),data.frame(aoa=aoa.fit))
Draco.pred <- data.frame(aoa=aoa.fit,Cl=predCl,Cd=predCd,Cm=predCm,LD=predCl/predCd)

predCl <- predict(loess(Cl~aoa,data=HalfWing),data.frame(aoa=aoa.fit))
predCd <- predict(loess(Cd~aoa,data=HalfWing),data.frame(aoa=aoa.fit))
predCm <- predict(loess(Cm~aoa,data=HalfWing),data.frame(aoa=aoa.fit))
HalfWing.pred <- data.frame(aoa=aoa.fit,Cl=predCl,Cd=predCd,Cm=predCm,LD=predCl/predCd)

predCl <- predict(loess(Cl~aoa,data=DoubleWing),data.frame(aoa=aoa.fit))
predCd <- predict(loess(Cd~aoa,data=DoubleWing),data.frame(aoa=aoa.fit))
predCm <- predict(loess(Cm~aoa,data=DoubleWing),data.frame(aoa=aoa.fit))
DoubleWing.pred <- data.frame(aoa=aoa.fit,Cl=predCl,Cd=predCd,Cm=predCm,LD=predCl/predCd)


quartz(width=3,height=2.5,pointsize=8)
par(mar=c(4,4,1,1),mgp=c(2.5,1,0))
plot(Cl~Cd,data=D,xlim=c(0,1.7),ylim=c(0,1.5),type="n",xlab="Drag coefficient",ylab="Lift coefficient")
abline(a=0,b=2.2,col="grey")
abline(0,0,col="grey")
points(Cl~Cd,data=DoubleWing,col="red")
points(Cl~Cd,data=Draco,col="green")
points(Cl~Cd,data=HalfWing,col="blue")
points(Cl~Cd,data=NoWing,col="magenta")
lines(Cl~Cd,data=DoubleWing.pred,col="red")
lines(Cl~Cd,data=Draco.pred,col="green")
lines(Cl~Cd,data=HalfWing.pred,col="blue")
lines(Cl~Cd,data=NoWing.pred,col="magenta")

quartz(width=3,height=2.5,pointsize=8)
par(mar=c(4,4,1,1),mgp=c(2.5,1,0))
plot(LD~aoa,data=D,xlim=c(-15,90),ylim=c(-1,4),type="n",xlab="Angle of attack, degrees",ylab="Lift to drag ratio")
abline(c(0,0),col="grey")
points(LD~aoa,data=DoubleWing,col="red")
points(LD~aoa,data=Draco,col="green")
points(LD~aoa,data=HalfWing,col="blue")
points(LD~aoa,data=NoWing,col="magenta")
lines(LD~aoa,data=DoubleWing.pred,col="red")
lines(LD~aoa,data=Draco.pred,col="green")
lines(LD~aoa,data=HalfWing.pred,col="blue")
lines(LD~aoa,data=NoWing.pred,col="magenta")

quartz(width=3,height=2.5,pointsize=8)
par(mar=c(4,4,1,1),mgp=c(2.5,1,0))
plot(-Cm~aoa,data=D,xlim=c(-15,90),ylim=c(-0.1,0.1),type="n",xlab="Angle of attack, degrees", ylab="Pitching moment coefficient")
abline(c(0,0),col="grey")
points(-Cm~aoa,data=DoubleWing,col="red")
points(-Cm~aoa,data=Draco,col="green")
points(-Cm~aoa,data=HalfWing,col="blue")
points(-Cm~aoa,data=NoWing,col="magenta")
lines(-Cm~aoa,data=DoubleWing.pred,col="red")
lines(-Cm~aoa,data=Draco.pred,col="green")
lines(-Cm~aoa,data=HalfWing.pred,col="blue")
lines(-Cm~aoa,data=NoWing.pred,col="magenta")
