#cd $HOME/docs/house-hunt_2011
#R CMD BATCH --no-save roomTemperatures.R

temps      <-read.table("temperature2.txt", sep = "\t",row.names=3, header=T)
tempsIns   <-read.table("temperature-insulated2.txt", sep = "\t",row.names=3, header=T)
tempsInsGlz<-read.table("temperature-insulated-glazed2.txt", sep = "\t",row.names=3, header=T)


pdf(file="roomTemperatures.pdf",  width=10, height=9)
op<-par(mfrow=c(1,1),cex=1.6,las=2,pin=c(7.5,6.0))
limits<-c(-2,20)
xlab = expression(paste("Outdoor temp (", degree, C, ")"))
ylab = expression(paste("Indoor temp (", degree, C, ")"))
plot(temps$outside_temp, temps$inside_temp,type="p",xlab=xlab,ylab=ylab,xlim=limits,ylim=limits,pch='x',main="Temperatures in the Gardner love-nest")
lm.T<-lm(temps$inside_temp ~ temps$outside_temp)
abline(lm.T)

x<-temps$outside_temp
y<-temps$inside_temp
nlmod <- nls(y ~  A / ( 1 + exp(C * x)), start=list(A=-10, C=0.5))
lines(x, predict(nlmod),lty=2)

points(tempsIns$outside_temp, tempsIns$inside_temp,col="red",pch='x')
lm.TIns<-lm(tempsIns$inside_temp ~ tempsIns$outside_temp)
abline(lm.TIns,col="red")

x<-tempsIns$outside_temp
y<-tempsIns$inside_temp
nlmod <- nls(y ~  A / ( 1 + exp(C * x)), start=list(A=21, C=1))
lines(x, predict(nlmod),lty=2,col="red")

points(tempsInsGlz$outside_temp, tempsInsGlz$inside_temp,col="orange",pch='x')
lm.TInsGlz<-lm(tempsInsGlz$inside_temp ~ tempsInsGlz$outside_temp)
abline(lm.TInsGlz,col="orange")

x<-tempsInsGlz$outside_temp
y<-tempsInsGlz$inside_temp
nlmod <- nls(y ~  A / ( 1 + exp(C * x)), start=list(A=21, C=1))
lines(x, predict(nlmod),lty=2,col="orange")

lines(c(-100,100),c(-100,100), lty=3, col="brown")
dX<-4.5
text(limits[1]+dX,limits[1]+dX, "Living outside", pos=4, col="brown")
lines(c(-100,100),c(20,20), lty=3, col="green")
text(limits[1]+dX,20-0.7, "Ideal room temp.", pos=4, col="green")
lines(c(-100,100),c(0,0), lty=3, col="blue")
text(limits[1]+dX, 0+0.5, "Fucking freezing!", pos=4, col="blue")
arrows(limits[1]+0.5, 0, x1=limits[1]+0.5, y1=limits[1], col="blue",lty=3)
arrows(limits[2]-0.5, 0, x1=limits[2]-0.5, y1=limits[1], col="blue",lty=3)
legend(8, 8, c("insulated & double glazed","insulated","uninsulated"),fill=c("orange","red","black"),cex=1.0)
dev.off()
