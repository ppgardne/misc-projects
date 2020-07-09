
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

#Scopus Sep 2015:
#Auckland H-index   = 236
#Otago H-index      = 203
#Canterbury H-index = 155
#Victoria H-index   = 136
#Massey H-index     = 153
#Waikato H-index    = 121
#Lincoln H-index    = 93
#AUT  H-index       = 74
#Cambridge H-index  = 

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
236,203,155,136,153,121,93,74
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


pdf(file="NZ_university_rankings.pdf", width=35, height=20)
op<-par(mfrow=c(1,1),las=1, cex = 3.0, bty='n')
par(fig=c(0.01,0.75,0.1,0.9),mai=c(0.5,2.75,0.5,0),las=2, new=TRUE,cex=2.75)
#       c(x1, x2, y1, y2)
plot(tbl$Year, tbl$Auck, type="l",col="cyan",lwd=5, xlab="Year", ylab="QS world ranking", main="NZ university rankings", ylim=rev(c(1,530)), xlim=c(2004,2019), xaxt="n", yaxt="n")
lines(tbl$Year, tbl$Otago, col="blue",lwd=5)
lines(tbl$Year, tbl$Cant, col="orange",lwd=5)
lines(tbl$Year, tbl$Vic, col="green",lwd=5)
lines(tbl$Year, tbl$Massey, col="black",lwd=5)
lines(tbl$Year, tbl$Waikato, col="red",lwd=5)
lines(tbl$Year, tbl$Lincoln, col="darkgreen",lwd=5)
lines(tbl$Year, tbl$AUT, col="darkred",lwd=5)
cols<-c("cyan","blue","orange","green","black","red","darkgreen", "darkred", "seagreen")

delta<-c(-15,5,-5,10,-15,5,5,-15)
text(2019.4,tbl[1,c(2:7,9,10)]+7+delta, labels = paste(colnames(tbl[,c(2:7,9,10)]),"-",tbl[tbl$Year==2019,c(2:7,9,10)],sep=" "), col=cols, pos=2)

#text(2015.4,405, labels = "Waikato: 401-410", col="red", pos=2)
#text(2015.4,420, labels = "Lincoln: 481-490", col="purple", pos=2)
#text(2015.4,420, labels = "AUT: 481-490", col="darkred", pos=2)

axis(1,at=c(2004:2019),labels=c(2004:2019), las=2)
axis(2,at=c(1,100,200,300,400,500),labels=c(1,100,200,300,400,500), las=2)
cols<-c(
"cyan",
"blue",
"orange",
"green",
"black",
"red","darkgreen", "darkred"
)

par(fig=c(0.75,0.99,0.1,0.9),mai=c(0.5,0.1,0.5,0),las=2, new=TRUE,cex=2.5)
barX<-barplot(hindexM, xlab="",names.arg=rev(c("Auckland","Otago","Canterbury","Victoria","Massey","Waikato","Lincoln","AUT")), col=c(rev(cols)), horiz='TRUE', main="2015 H-index", yaxt="n")
text(x=175,y=barX, label=paste(hindex), col=c("black","black","black","black","black","black","white","black"), cex=1)

#par(fig=c(0.83,0.99,0.1,0.9),mai=c(0.5,0.1,0.5,0),las=2, new=TRUE,cex=2.5)
#barX<-barplot(h6indexM, xlab="",names.arg=rev(c("Auckland","Otago","Canterbury","Victoria","Massey","Waikato","Lincoln")), col=c("purple",rev(cols)), horiz='TRUE', main="2013 H6-index", yaxt="n")
#text(x=80,y=barX, label=paste(h6index), col=c("black","black","black","black","black","white","black"), cex=1)
dev.off()


png(file="NZ_university_rankings-ppt.png", width=880, height=880)
op<-par(mfrow=c(1,1),las=1, cex = 3) #mai:c(bottom, left, top, right)
#par(las=2, new=TRUE,cex=2.75)
#       c(x1, x2, y1, y2)
plot(tbl$Year, tbl$Auck, type="l",col="cyan",lwd=5, xlab="Year", ylab="QS world ranking", main="NZ university rankings", ylim=rev(c(1,420)), xlim=c(2004,2015), xaxt="n", yaxt="n")
lines(tbl$Year, tbl$Otago, col="blue",lwd=5)
lines(tbl$Year, tbl$Cant, col="orange",lwd=5)
lines(tbl$Year, tbl$Vic, col="green",lwd=5)
lines(tbl$Year, tbl$Massey, col="black",lwd=5)
lines(tbl$Year, tbl$Waikato, col="red",lwd=5)
cols<-c("cyan","blue","orange","green","black","red","seagreen")
t2<-c(0,0,0,0,0)
text(2015.4,tbl[1,2:6]+7, labels = paste(colnames(tbl[,2:6]),"-",tbl[tbl$Year==2015,2:6],sep=" "), col=cols, pos=2)
text(2015.4,380, labels = "Waikato: 401-410", col="red", pos=2)
text(2015.4,420, labels = "Lincoln: 481-490", col="purple", pos=2)
axis(1,at=c(2004:2015),labels=c(2004:2015), las=2)
axis(2,at=c(1,100,200,300,400),labels=c(1,100,200,300,400), las=2)
cols<-c(
"cyan",
"blue",
"orange",
"green",
"black",
"red"
)
dev.off()



#####EFTS
#http://www.universitiesnz.ac.nz/nz-university-system

#efts<-read.table(file="NZ_university_EFTS_2011.txt",       row.names=1, header=TRUE, sep="\t")
efts<-read.table(file="NZ_university_EFTS_2011-cites.txt", row.names=1, header=TRUE, sep="\t")

low<-4
staffStudRat<-efts$Students.EFTS/efts$Staff.FTE-low
#staffStudRat<-rev(staffStudRat)

staffStudRatM<-mat.or.vec(length(staffStudRat), length(staffStudRat))
for (i in seq(1,length(staffStudRat))){
    staffStudRatM[i,i]<-staffStudRat[i]
}
colnames(staffStudRatM)<-rownames(efts)
rownames(staffStudRatM)<-rownames(efts)

staffM<-mat.or.vec(length(efts$Staff.FTE), length(efts$Staff.FTE))
for (i in seq(1,length(efts$Staff.FTE))){
    staffM[i,i]<-efts$Staff.FTE[i]
}
colnames(staffM)<-rownames(efts)
rownames(staffM)<-rownames(efts)

studentsM<-mat.or.vec(length(efts$Students.EFTS), length(efts$Students.EFTS))
for (i in seq(1,length(efts$Students.EFTS))){
    studentsM[i,i]<-efts$Students.EFTS[i]
}
colnames(studentsM)<-rownames(efts)
rownames(studentsM)<-rownames(efts)

citesM<-mat.or.vec(length(efts$Citations.2008.2012), length(efts$Citations.2008.2012))
for (i in seq(1,length(efts$Citations.2008.2012))){
    citesM[i,i]<-efts$Citations.2008.2012[i]
}
colnames(citesM)<-rownames(efts)
rownames(citesM)<-rownames(efts)

citesPerFacM<-mat.or.vec(length(efts$Citations.2008.2012), length(efts$Citations.2008.2012))
for (i in seq(1,length(efts$Citations.2008.2012))){
    citesPerFacM[i,i]<-efts$Citations.2008.2012[i]/efts$Staff.FTE[i]
}
colnames(citesPerFacM)<-rownames(efts)
rownames(citesPerFacM)<-rownames(efts)


pdf(file="NZ_student_staff_ratio.pdf", width=20, height=20)
op<-par(mfrow=c(1,1),las=2, cex = 3.0, bty='n')
par(fig=c(0.01,0.49,0.1,0.9),mai=c(0.5,2.75,0.5,0),las=2, new=TRUE,cex=2.75)
barX<-barplot(staffStudRatM, xlab="Number of students per staff member", horiz='TRUE', main="2011 Student/Staff Ratios",col=c("white","purple",rev(cols)), xaxt="n", yaxt="n")
axis(1,at=c(low:9-low),labels=c(low:9), las=1)
text(x=4-low,y=barX, label=paste(rownames(efts)), col=c("black","white","black","white","black","black","white","black"), cex=1, pos=4)
#
par(fig=c(0.50,0.99,0.1,0.9),mai=c(0.5,2.75,0.5,0),las=2, new=TRUE,cex=2.75)
barX<-barplot(citesPerFacM, xlab="Citation/faculty", horiz='TRUE', main="Citation/faculty 2008-2012",col=c("white","purple",rev(cols)), yaxt="n")
#
dev.off()

png(file="NZ_student_staff_ratio-ppt.png", width=1600, height=880)
op<-par(mfrow=c(1,1),las=2, cex = 1.0, bty='n')
par(fig=c(0.01,0.49,0.1,0.9),mai=c(0.5,2.75,0.5,0),las=2, new=TRUE,cex=2.75)
barX<-barplot(staffStudRatM, xlab="Number of students per staff member", horiz='TRUE', main="2011 Student/Staff Ratios",col=c("white","purple",rev(cols)), xaxt="n", yaxt="n")
axis(1,at=c(low:9-low),labels=c(low:9), las=1)
text(x=4-low,y=barX, label=paste(rownames(efts)), col=c("black","white","black","white","black","black","white","black"), cex=1.25, pos=4)
#
par(fig=c(0.50,0.99,0.1,0.9),mai=c(0.5,2.75,0.5,0),las=2, new=TRUE,cex=2.75)
barX<-barplot(citesPerFacM, xlab="Citation/faculty", horiz='TRUE', main="Citation/faculty 2008-2012",col=c("white","purple",rev(cols)), yaxt="n")
#
dev.off()


pdf(file="NZ_student_staff_data.pdf", width=35, height=20)
op<-par(mfrow=c(1,1),las=2, cex = 3.0, bty='n')
par(fig=c(0.01,0.32,0.1,0.9),mai=c(0.5,2.75,0.5,0),las=2, new=TRUE,cex=2.75)
barX<-barplot(staffM, xlab="FTEs", horiz='TRUE', main="2011 FTE Staff",col=c("white","purple",rev(cols)), yaxt="n")
text(x=4-low,y=barX, label=paste(rownames(efts)), col=c("black","white","black","white","black","black","white","black"), cex=1, pos=4)
#
par(fig=c(0.33,0.65,0.1,0.9),mai=c(0.5,2.75,0.5,0),las=2, new=TRUE,cex=2.75)
barX<-barplot(studentsM, xlab="FTEs", horiz='TRUE', main="2011 Student EFTS",col=c("white","purple",rev(cols)), yaxt="n")
#
par(fig=c(0.66,0.99,0.1,0.9),mai=c(0.5,2.75,0.5,0),las=2, new=TRUE,cex=2.75)
barX<-barplot(citesM, xlab="Citations (2008-2012)", horiz='TRUE', main="Citations",col=c("white","purple",rev(cols)), yaxt="n")
#
dev.off()


