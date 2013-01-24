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

#Scopus Sep 2012:
#Auckland H-index   = 175
#Otago H-index      = 190
#Canterbury H-index = 117
#Victoria H-index   = 101
#Massey H-index     = 114 
#Waikato H-index    = 89
#Lincoln H-index    = 68
#Cambridge H-index  = 425

hindex<-c(
#425,
175,
190,
117,
101,
114,
89,
68
)
hindex<-rev(hindex)

hindexM<-mat.or.vec(length(hindex), length(hindex))
for (i in seq(1,length(hindex))){
    hindexM[i,i]<-hindex[i]
}


pdf(file="NZ_university_rankings.pdf", width=25, height=20)

op<-par(mfrow=c(1,1),las=1, cex = 3.0, bty='n')

par(fig=c(0.01,0.92,0.1,0.9),mai=c(0.5,2.75,0.5,0),las=2, new=TRUE,cex=2.75)

plot(tbl$Year, tbl$Auck, type="l",col="cyan",lwd=5, xlab="Year", ylab="QS world ranking", main="NZ university rankings", ylim=rev(c(1,400)), xlim=c(2004,2013), xaxt="n", yaxt="n")
#lines(tbl$Year, tbl$Cambridge, col="seagreen",lwd=5)
lines(tbl$Year, tbl$Otago, col="blue",lwd=5)
lines(tbl$Year, tbl$Cant, col="orange",lwd=5)
lines(tbl$Year, tbl$Vic, col="green",lwd=5)
lines(tbl$Year, tbl$Massey, col="black",lwd=5)
lines(tbl$Year, tbl$Waikato, col="red",lwd=5)
#legend(2006,130,colnames(tbl[,2:7]),col=c("cyan","blue","orange","green","black","red"))
cols<-c("cyan","blue","orange","green","black","red","seagreen")
t1<-c(0,-10,-10,0,10,10)
text(2004,  tbl[8,2:7]+t1, labels = paste(colnames(tbl[,2:7]),"-",tbl[tbl$Year==2005,2:7],sep=" "), col=cols, pos=4)
t2<-c(0,0,0,0,0,20)
text(2012.4,tbl[1,2:7]+t2, labels = paste(colnames(tbl[,2:7]),"-",tbl[tbl$Year==2012,2:7],sep=" "), col=cols, pos=2)
text(2012.4,410, labels = "Lincoln - <500", col="purple", pos=2)
#text(2012,   5,   labels = "Cambridge", col="seagreen", pos=3)
#text(2004.25,5,   labels = "Cambridge", col="seagreen", pos=3)

axis(1,at=c(2004:2012),labels=c(2004:2012), las=2)
axis(2,at=c(1,100,200,300,400),labels=c(1,100,200,300,400), las=2)

cols<-c(
#"seagreen",        
"cyan",
"blue",
"orange",
"green",
"black",
"red"
)

par(fig=c(0.85,0.98,0.1,0.9),mai=c(0.5,0.1,0.5,0),las=2, new=TRUE,cex=2.5)
#fig=c(x1, x2, y1, y2)
#barplot(hindexM, xlab="",names.arg=rev(c("Cambridge","Auckland","Otago","Canterbury","Victoria","Massey","Waikato","Lincoln")), col=c("purple",rev(cols)), horiz='TRUE', main="2012 H-index", yaxt="n")
barX<-barplot(hindexM, xlab="",names.arg=rev(c("Auckland","Otago","Canterbury","Victoria","Massey","Waikato","Lincoln")), col=c("purple",rev(cols)), horiz='TRUE', main="2012 H-index", yaxt="n")
text(x=140,y=barX, label=paste(hindex), col=c("black","black","black","black","black","white","black"), cex=1)

dev.off()


