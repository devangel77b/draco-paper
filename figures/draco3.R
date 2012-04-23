# Load all the Draco data
D <- read.table('All Draco.txt',header=TRUE)

maximus <- D[(D$model == 'Draco' | D$model == 'Draco2') & (D$v==7 | D$v == 5.6),]
melanopogon <- D[D$model=='Draco' & D$v==3,]
allflat <- rbind(maximus,melanopogon)
superdraco <- D[D$model=='Draco' & D$v==12,]
camber <- D[(D$model == 'Camber'),]

LD <- allflat$L/allflat$D
LDc <- camber$L/camber$D

#quartz('LDratio',width=3,height=2.5,pointsize=8,file="LD vs AOA.pdf")
par(mar=c(4,4,1,1),mgp=c(2.5,1,0))
plot(allflat$aoa,LD,ylim=c(-1,3.5),type="n",xlab="Angle of attack, degrees",ylab="Lift to drag ratio")
points(allflat$aoa,LD,pch=21,col="red",bg="red")
points(camber$aoa,LDc,pch=23,col="blue",bg="blue")

quantile(LD,0.95)
quantile(LDc,0.95)
atan(1/quantile(LD,0.95))*180/pi
atan(1/quantile(LDc,0.95))*180/pi

# The 95th quantile is 2.7 for flat, 2.5 for cambered
# These correspond to glide angles of 20.3 and 21.8 degrees. 
# The absolute maxes are 3.2 and 3.1 respectively
# or 17.0 and 17.7 degrees.  
# Trajectories are about this overall... but
# the trajectories do not match these angles instantaneously
# - they're doing something different... nonequilibrium glides.  

# What if I check this?
#plot(allflat$aoa,atan(1/LD)*180/pi)
