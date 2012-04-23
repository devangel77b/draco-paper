# Load all the Draco data
D <- read.table('All Draco.txt',header=TRUE)

D.Re <- subset(D,(aoa==20 & model == 'Draco'))
D.Re.fit <- D.Re[D.Re$Re>40000,]
Clfit <- lm(D.Re.fit$Cl~Re,data=D.Re.fit)
Cdfit <- lm(D.Re.fit$Cd~Re,data=D.Re.fit)
Cmfit <- lm(D.Re.fit$Cm~Re,data=D.Re.fit)
smRe <- seq(40000,150000,by=10000)

quartz('Lift and drag coefficient dependence on Re',width=3,height=2.5,pointsize=8,file="CL and CD vs Re.pdf")
par(mar=c(4,4,1,1),mgp=c(2.5,1,0))
plot(D.Re$Re,D.Re$Cl,log="",xlim=c(15000,120000),ylim=c(0,1),las=1,type="n",xlab="Reynolds number",ylab="Nondimensional coefficient");
matlines(smRe,predict(Clfit,data.frame(Re=smRe),interval="prediction"),col="darkgray",lty=c(1,2,2))
matlines(smRe,predict(Cdfit,data.frame(Re=smRe),interval="prediction"),col="darkgrey",lty=c(1,2,2))
matlines(smRe,predict(Cmfit,data.frame(Re=smRe),interval="prediction"),col="darkgrey",lty=c(1,2,2))
points(D.Re$Re,D.Re$Cd,pch=22,bg="black")
points(D.Re$Re,D.Re$Cl,pch=24)
points(D.Re$Re,D.Re$Cm,pch=23)

summary(Clfit)
summary(Cdfit)
summary(Cmfit)