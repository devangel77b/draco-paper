# Load all the Draco data
D <- read.table('All Draco.txt',header=TRUE)

D$LD <- D$L/D$D
NoWing <- D[D$model=='NoWing' & D$aoa==20,]
HalfWing <- D[D$model=='HalfWing' & D$aoa==20,]
Draco <- D[(D$model=='Draco' | D$model == 'Draco2') & D$aoa==20,]
DoubleWing <- D[D$model=='DoubleWing' & D$aoa==20,]

# Make predictions
Re.fit <- data.frame(Re=seq(0,120000,1000))
DoubleWing.Lpred <- predict(lm(L~poly(Re,3),data=DoubleWing),Re.fit)
Draco.Lpred <- predict(lm(L~poly(Re,3),data=Draco),Re.fit)
HalfWing.Lpred <- predict(lm(L~poly(Re,3),data=HalfWing),Re.fit)
NoWing.Lpred <- predict(lm(L~poly(Re,3),data=NoWing),Re.fit)
DoubleWing.Dpred <- predict(lm(D~poly(Re,3),data=DoubleWing),Re.fit)
Draco.Dpred <- predict(lm(D~poly(Re,3),data=Draco),Re.fit)
HalfWing.Dpred <- predict(lm(D~poly(Re,3),data=HalfWing),Re.fit)
NoWing.Dpred <- predict(lm(D~poly(Re,3),data=NoWing),Re.fit)

DoubleWing.pred <- data.frame(Re=seq(0,120000,1000),L=DoubleWing.Lpred,D=DoubleWing.Dpred)
Draco.pred <- data.frame(Re=seq(0,120000,1000),L=Draco.Lpred,D=Draco.Dpred)
HalfWing.pred <- data.frame(Re=seq(0,120000,1000),L=HalfWing.Lpred,D=HalfWing.Dpred)
NoWing.pred <- data.frame(Re=seq(0,120000,1000),L=NoWing.Lpred,D=NoWing.Dpred)





quartz(width=3,height=2.5,pointsize=8)
par(mar=c(4,4,1,1),mgp=c(2.5,1,0))
plot(L~Re,data=D,type="n",xlab="Reynolds number",ylab="Lift, N",las=1)
points(L~Re,data=DoubleWing,col="red")
points(L~Re,data=Draco,col="green")
points(L~Re,data=HalfWing,col="blue")
points(L~Re,data=NoWing,col="magenta")
lines(L~Re,data=DoubleWing.pred,col="red")
lines(L~Re,data=Draco.pred,col="green")
lines(L~Re,data=HalfWing.pred,col="blue")
lines(L~Re,data=NoWing.pred,col="magenta")


quartz(width=3,height=2.5,pointsize=8)
par(mar=c(4,4,1,1),mgp=c(2.5,1,0))
plot(LD~Re,data=D,type="n",ylim=c(-1,3),xlab="Reynolds number",ylab="Lift-to-drag ratio",las=1)
points(LD~Re,data=DoubleWing,col="red")
points(LD~Re,data=Draco,col="green")
points(LD~Re,data=HalfWing,col="blue")
points(LD~Re,data=NoWing,col="magenta")
lines(L/D~Re,data=DoubleWing.pred[DoubleWing.pred$Re>40000,],col="red")
lines(L/D~Re,data=Draco.pred[Draco.pred$Re>40000,],col="green")
lines(L/D~Re,data=HalfWing.pred[HalfWing.pred$Re>40000,],col="blue")
lines(L/D~Re,data=NoWing.pred[NoWing.pred$Re>40000,],col="magenta")

