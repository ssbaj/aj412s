## 모든 변수들의 평균/표준편차/변동계수 등을 구하는 함수

desc22<-function(A0data, digitx=3){
options(scipen=100)
options(digits=digitx)

if (base::missing(A0data)) {
	    return(cat("  desc2(df) "))  }

## coverting character data into numeric variables
c2n <- function(x){   
	 groups = unique(x)   
     groups= sort(groups)
     tmp<-as.numeric(factor(x, levels=groups))
	 return(tmp)}


# START Data Setup ------------------

A0data<-as.data.frame(A0data)
nc <- ncol(A0data)

if(nc==1) {
if( class(A0data$A0data)=="character" ) A0data$A0data<-c2n(A0data$A0data)
tmp_onevar<-A0data$A0data
r_nX<-sum( !is.na(tmp_onevar) )
r_NAX <- sum( is.na(tmp_onevar) )
r_zeros <- sum( tmp_onevar==0 , na.rm=T)
r_nonzeros <- sum( tmp_onevar!=0 , na.rm=T)
r_median<-median(tmp_onevar, na.rm=T)
r_mean<-mean(tmp_onevar, na.rm=T)
r_sd<-sd(tmp_onevar, na.rm=T)
r_14<-quantile(tmp_onevar, c(.25), na.rm=T)
r_24<-quantile(tmp_onevar, c(.5), na.rm=T)
r_34<-quantile(tmp_onevar, c(.75), na.rm=T)
r_min<-min(tmp_onevar, na.rm=T)
r_max<-max(tmp_onevar, na.rm=T)
r_sm<-sd(tmp_onevar, na.rm=T)/mean(tmp_onevar, na.rm=T)
}


if(nc>1) {
for(j in 1:nc){
if( class(A0data[,j])=="character" ) A0data[,j]<-c2n(A0data[,j])
}

## coverting data into numeric variables
index2<-c(1:nc)
A0data[,index2] <- lapply( A0data[, index2], as.numeric)

## Variables for saving
r_mean<-c(); r_sd<-c(); r_14<-c(); r_24<-c(); r_34<-c(); r_min<-c(); 
r_max<-c(); r_sm<-c();r_nX<-c(); r_NAX<-c(); r_median<-c()
r_zeros<-c(); r_nonzeros<-c()

for(ix00 in 1:nc){
t_nX<-sum( !is.na(A0data[,ix00]) )
t_NAX <- sum( is.na(A0data[,ix00]) )
t_zeros <- sum( A0data[,ix00]==0 , na.rm=T)
t_nonzeros <- sum( A0data[,ix00]!=0 , na.rm=T)
t_median<-median(A0data[,ix00], na.rm=T)
t_mean<-round( mean(A0data[,ix00], na.rm=T), digitx)
t_sd<-sd(A0data[,ix00], na.rm=T)
t_14<-quantile(A0data[,ix00], c(.25), na.rm=T)
t_24<-quantile(A0data[,ix00], c(.5), na.rm=T)
t_34<-quantile(A0data[,ix00], c(.75), na.rm=T)
t_min<-min(A0data[,ix00], na.rm=T)
t_max<-max(A0data[,ix00], na.rm=T)
t_sm<- round( sd(A0data[,ix00], na.rm=T)/mean(A0data[,ix00], na.rm=T) , digitx)

r_nX<-c(r_nX, t_nX)
r_NAX<-c(r_NAX, t_NAX)
r_zeros<-c(r_zeros, t_zeros)
r_nonzeros<-c(r_nonzeros, t_nonzeros)
r_median=c(r_median, t_median) 
r_mean=c(r_mean, t_mean) 
r_sd=c(r_sd, t_sd)
r_14=c(r_14, t_14)
r_24=c(r_24, t_24)
r_34=c(r_34, t_34) 
r_min=c(r_min, t_min) 
r_max=c(r_max, t_max) 
r_sm=c(r_sm, t_sm) 

}
}

if(nc==1){
r0<-'var'
}

if(nc>1){
r0<-(colnames(A0data))
}


r1<-(r_nX)
r2<-(r_NAX)
r3<-(r_zeros)
r4<-(r_nonzeros)
r5<-(r_median)
r6<-(r_mean)
r7<-(r_sd)
r8<-(r_14)
r9<-(r_24)
r10<-(r_34)
r11<-(r_min)
r12<-(r_max)
r13<-(r_sm)

Bdata<-rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13)
Bdata<-t(Bdata)
Bdata<-as.data.frame(Bdata)
rownames(Bdata)<-r0

## Finish data setup --------------------------

colnames(Bdata)[1] <- c('  Ndata')
colnames(Bdata)[2] <- c('  NAs')
colnames(Bdata)[3] <- c('  zero')
colnames(Bdata)[4] <- c('  nonzero')
colnames(Bdata)[5] <- c('  Median')
colnames(Bdata)[6] <- c('  Mean')
colnames(Bdata)[7] <- c('St.Dev.')
colnames(Bdata)[8] <- c('Q1')
colnames(Bdata)[9] <- c('Q2')
colnames(Bdata)[10] <- c('Q3')
colnames(Bdata)[11] <- c('  Min')
colnames(Bdata)[12] <- c('  Max')
colnames(Bdata)[13] <- c('  St.Dev/Mean')

return(Bdata)
}
