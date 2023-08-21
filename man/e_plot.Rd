\name{e_plot}
\alias{e_plot}
\title{plotting graph}
\usage{
e_plot()
}
\description{
Plotting Graphs
}
\examples{
library(readxl)
-------------------------------------------------
Adata<-read_excel('Data41_Time_series_data3.xlsx', skip=1) 
Adata$yearmonth<-paste0(Adata$yearmonth, '-01')
# Adata$yearmonth<-as.Date(Adata$yearmonth, format='%Y-%m-%d')
plot(Adata$yearmonth, Adata$surplus, type='l', col='red', xlab='',ylab='', cex.lab=1.3)
par(new=TRUE)
plot(Adata$yearmonth, Adata$kospi, type='l', col='blue', xlab='',ylab='', axes=FALSE)
axis(side = 4, col = 'blue')
mtext("KOSPI지수", side = 4, col='blue' , cex.lab=1.3)
title(main='경상수지와 코스피지수',xlab='시간',ylab='경상수지', cex.lab=1.3, cex.main=1.3)
-------------------------------------------------
library(readxl)
Adata<-read_excel('Data42_Suwon.xlsx')
colnames(Adata)<-c('date0','index','budget','rev','accrev','exp','accexp','slack')
Adata$date0<-as.Date(Adata$date0)
Adata$accrev<-Adata$accrev/1000000000
Adata$accexp<-Adata$accexp/1000000000
Adata$slack<-Adata$slack/1000000000

plot(x=Adata$date0, y=Adata$accrev, type='l', lwd='3', lty='dotted', col='red', xlab='',ylab='', ylim=c(10, 4000))
par(new=TRUE)
plot(x=Adata$date0, y=Adata$accexp, type='l', lwd='3', lty='twodash', col='blue', xlab='', ylab='', ylim=c(10, 4000))
par(new=TRUE)
plot(x=Adata$date0, y=Adata$slack, type='l', lwd='3', lty='solid', col='salmon3', xlab='', ylab='', ylim=c(10, 4000))
title(main='세입/세출/여유자원  ', xlab='시간', ylab='세입(Red)/세출(Blue)/여유자산(Brown)/단위:10억', cex.main=2, cex.lab=1.2)


}