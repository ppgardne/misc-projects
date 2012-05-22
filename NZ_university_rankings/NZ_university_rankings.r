tbl<-read.table(file="NZ_university_rankings.txt", header=TRUE, sep="\t")

#Scopus June 2011:
#Auckland H-index   = 174
#Otago H-index      = 166
#Canterbury H-index = 104
#Victoria H-index   = 89
#Massey H-index     = 105 
#Waikato H-index    = 79
## hindex<-c(
## 174,
## 166,
## 104,
## 89,
## 105,
## 79
## )

#Scopus May 2012:
#Auckland H-index   = 186
#Otago H-index      = 186
#Canterbury H-index = 114
#Victoria H-index   = 98
#Massey H-index     = 111 
#Waikato H-index    = 86


hindex<-c(
186,
186,
114,
98,
111,
86
)
hindex<-rev(hindex)

hindexM<-mat.or.vec(length(hindex), length(hindex))
for (i in seq(1,length(hindex))){
    hindexM[i,i]<-hindex[i]
}


pdf(file="NZ_university_rankings.pdf")

op<-par(mfrow=c(1,1),las=1, bty='n')

plot(tbl$Year, tbl$Auck, type="l",col="cyan",lwd=3, xlab="Year", ylab="QS world ranking", main="NZ university rankings", ylim=rev(c(1,400)), xlim=c(2004,2013), xaxt="n")
lines(tbl$Year, tbl$Otago, col="blue",lwd=3)
lines(tbl$Year, tbl$Cant, col="orange",lwd=3)
lines(tbl$Year, tbl$Vic, col="green",lwd=3)
lines(tbl$Year, tbl$Massey, col="black",lwd=3)
lines(tbl$Year, tbl$Waikato, col="red",lwd=3)
#legend(2006,130,colnames(tbl[,2:7]),col=c("cyan","blue","orange","green","black","red"))
cols<-c("cyan","blue","orange","green","black","red")
t1<-c(0,0,0,20,0,20)
text(2004.15,tbl[6,2:7]+t1, labels = colnames(tbl[,2:7]), col=cols, pos=3)
t2<-c(0,0,0,0,0,20)
text(2011,tbl[1,2:7]+t2, labels = colnames(tbl[,2:7]), col=cols, pos=3)

axis(1,at=c(2004:2011),labels=c(2004:2011), las=1)

cols<-c(
"cyan",
"blue",
"orange",
"green",
"black",
"red"
)

par(fig=c(0.8,0.98,0.15,0.85),mai=c(0.5,0.1,0.29,0),las=1, new=TRUE,cex=0.9)
#fig=c(x1, x2, y1, y2)
barplot(hindexM, xlab="",names.arg=rev(c("Auckland","Otago","Canterbury","Victoria","Massey","Waikato")), col=rev(cols), horiz='TRUE', main="2012 H-index", yaxt="n")


dev.off()


