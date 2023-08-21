## 모든 변수들의 평균을 구하는 함수

desc2<-function(A0_data, mydigits=2){

options(digits=mydigits)


if (base::missing(A0_data)) {
	    return(cat("  desc2(df) "))  }


A0_data<-as.data.frame(A0_data)
nc <- ncol(A0_data)


## coverting character data into numeric variables
c2n <- function(x){   
	 groups = unique(x)   
     groups= sort(groups)
     tmp<-as.numeric(factor(x, levels=groups))
	 return(tmp)}


for(j in 1:nc){
if( class(A0_data[,j])=="character" ) A0_data[,j]<-c2n(A0_data[,j])
}


## coverting data into numeric variables
index2<-c(1:nc)
A0_data[,index2] <- lapply( A0_data[, index2], as.numeric)


## Variables for saving
r_mean<-c(); r_sd<-c(); r_14<-c(); r_24<-c(); r_34<-c(); r_min<-c(); 
r_max<-c(); r_sm<-c();r_nX<-c(); r_NAX<-c(); r_median<-c()


for(ix00 in 1:nc){
t_nX<-sum( !is.na(A0_data[,ix00]) )
t_NAX <- sum( is.na(A0_data[,ix00]) )
t_median<-median(A0_data[,ix00], na.rm=T)
t_mean<-mean(A0_data[,ix00], na.rm=T)
t_sd<-sd(A0_data[,ix00], na.rm=T)
t_14<-quantile(A0_data[,ix00], c(.25), na.rm=T)
t_24<-quantile(A0_data[,ix00], c(.5), na.rm=T)
t_34<-quantile(A0_data[,ix00], c(.75), na.rm=T)
t_min<-min(A0_data[,ix00], na.rm=T)
t_max<-max(A0_data[,ix00], na.rm=T)
t_sm<-sd(A0_data[,ix00], na.rm=T)/mean(A0_data[,ix00], na.rm=T)

r_nX<-c(r_nX, t_nX)
r_NAX<-c(r_NAX, t_NAX)
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



r0<-as.vector(colnames(A0_data))
r_nX<-as.vector(r_nX);
r_NAX<-as.vector(r_NAX);
r_median<-as.vector(r_median);
r_mean<-as.vector(r_mean)
r_sd<-as.vector(r_sd);  
r_14<-as.vector(r_14)
r_24<-as.vector(r_24)
r_34<-as.vector(r_34);  
r_min<-as.vector(r_min);
r_max<-as.vector(r_max);
r_sm<-as.vector(r_sm);

Bdata <- rbind(r0, r_nX, r_NAX, r_median, r_mean, r_sd, r_14, r_24, r_34, r_min, r_max, r_sm)
Bdata<-as.data.frame(Bdata)
colnames(Bdata)<-Bdata[1,]
Bdata<-Bdata[-c(1),]
Bdata<-as.data.frame(Bdata)
nc2<-ncol(Bdata)
index<-c(1:nc2)
Bdata[ , index] <- lapply( Bdata[ , index], as.numeric )

rownames(Bdata)[1] <- c('  Ndata')
rownames(Bdata)[2] <- c('  NAs')
rownames(Bdata)[3] <- c('  Median')
rownames(Bdata)[4] <- c('  Mean')
rownames(Bdata)[5] <- c('St.Dev.')
rownames(Bdata)[6] <- c('Q1')
rownames(Bdata)[7] <- c('Q2')
rownames(Bdata)[8] <- c('Q3')
rownames(Bdata)[9] <- c('  Min')
rownames(Bdata)[10] <- c('  Max')
rownames(Bdata)[11] <- c('    SD/Mean')

return(t(Bdata))
}



