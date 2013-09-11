
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

#Scopus Sep 2013:
#Auckland H-index   = 196
#Otago H-index      = 203
#Canterbury H-index = 128
#Victoria H-index   = 111
#Massey H-index     = 128
#Waikato H-index    = 96
#Lincoln H-index    = 75
#Cambridge H-index  = 458

#Scopus Sep 2007-2013:
#Auckland H6-index   = 112
#Otago H6-index      = 100
#Canterbury H6-index = 67
#Victoria H6-index   = 58
#Massey H6-index     = 67
#Waikato H6-index    = 45
#Lincoln H6-index    = 40

tbl<-read.table(file="NZ_university_rankings.txt", header=TRUE, sep="\t")

hindex<-c(
    196,
    203,
    128,
    111,
    128,
    96,
    75
    )
hindex<-rev(hindex)
hindexM<-mat.or.vec(length(hindex), length(hindex))
for (i in seq(1,length(hindex))){
    hindexM[i,i]<-hindex[i]
}


h6index<-c(
    112,
    100,
    67,
    58,
    67,
    45,
    40
    )
h6index<-rev(h6index)

h6indexM<-mat.or.vec(length(h6index), length(h6index))
for (i in seq(1,length(h6index))){
    h6indexM[i,i]<-h6index[i]
}


pdf(file="NZ_university_rankings.pdf", width=30, height=20)
op<-par(mfrow=c(1,1),las=1, cex = 3.0, bty='n')
par(fig=c(0.01,0.66,0.1,0.9),mai=c(0.5,2.75,0.5,0),las=2, new=TRUE,cex=2.75)
#       c(x1, x2, y1, y2)
plot(tbl$Year, tbl$Auck, type="l",col="cyan",lwd=5, xlab="Year", ylab="QS world ranking", main="NZ university rankings", ylim=rev(c(1,420)), xlim=c(2004,2013), xaxt="n", yaxt="n")
lines(tbl$Year, tbl$Otago, col="blue",lwd=5)
lines(tbl$Year, tbl$Cant, col="orange",lwd=5)
lines(tbl$Year, tbl$Vic, col="green",lwd=5)
lines(tbl$Year, tbl$Massey, col="black",lwd=5)
lines(tbl$Year, tbl$Waikato, col="red",lwd=5)
cols<-c("cyan","blue","orange","green","black","red","seagreen")

t2<-c(0,0,0,0,0)
text(2013.4,tbl[1,2:6]+7, labels = paste(colnames(tbl[,2:6]),"-",tbl[tbl$Year==2013,2:6],sep=" "), col=cols, pos=2)

text(2013.4,405, labels = "Waikato: 401-410", col="red", pos=2)
text(2013.4,420, labels = "Lincoln: 481-490", col="purple", pos=2)

axis(1,at=c(2004:2013),labels=c(2004:2013), las=2)
axis(2,at=c(1,100,200,300,400),labels=c(1,100,200,300,400), las=2)
cols<-c(
"cyan",
"blue",
"orange",
"green",
"black",
"red"
)

par(fig=c(0.66,0.82,0.1,0.9),mai=c(0.5,0.1,0.5,0),las=2, new=TRUE,cex=2.5)
barX<-barplot(hindexM, xlab="",names.arg=rev(c("Auckland","Otago","Canterbury","Victoria","Massey","Waikato","Lincoln")), col=c("purple",rev(cols)), horiz='TRUE', main="2013 H-index", yaxt="n")
text(x=150,y=barX, label=paste(hindex), col=c("black","black","black","black","black","white","black"), cex=1)

par(fig=c(0.83,0.99,0.1,0.9),mai=c(0.5,0.1,0.5,0),las=2, new=TRUE,cex=2.5)
barX<-barplot(h6indexM, xlab="",names.arg=rev(c("Auckland","Otago","Canterbury","Victoria","Massey","Waikato","Lincoln")), col=c("purple",rev(cols)), horiz='TRUE', main="2013 H6-index", yaxt="n")
text(x=80,y=barX, label=paste(h6index), col=c("black","black","black","black","black","white","black"), cex=1)
dev.off()


