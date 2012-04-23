# futzing around with R and some Draco data

D <- read.table('All Draco.txt',header=TRUE)
theorder <- 10

# now try to fit the baseline
D.baseline <- subset(D,((model=='Draco') & (v == 7)))
plot(D.baseline$aoa,D.baseline$Cl,xlab="Angle of attack, deg",ylab="Lift coefficient",col="white",pch=21,bg="blue",ylim=c(-0.2,1.2))
poly.baseline <- lm(Cl~poly(aoa,order=theorder),data=D.baseline)
xfit <- seq(-15,90,by=0.5)
lines(xfit,predict(poly.baseline,newdata=data.frame(aoa=xfit)),col=4)
summary(poly.baseline)


# compare baseline and camber
D.camber <- subset(D,((model=='Camber') & (v == 7)))
points(D.camber$aoa,D.camber$Cl,xlab="Angle of attack, deg",ylab="Lift coefficient",col="white",pch=21,bg="red")
poly.camber <- lm(Cl~poly(aoa,order=theorder),data=D.camber)
xfit <- seq(-15,90,by=0.5)
lines(xfit,predict(poly.camber,newdata=data.frame(aoa=xfit)),col="red")
summary(poly.camber)
