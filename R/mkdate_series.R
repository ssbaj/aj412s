mkdate_series<-function(df, start_y, start_m=0, mq=0 ){

if (base::missing(df)) {
	    cat("  df<-as.data.frame(df) ", '\n')
		cat("  Input: 연도=2015, start_m=11 / start_m=0(Yearly DATA),", '\n')
		cat("                    연간자료: mq=0 / 분기별자료: mq=4 / 월별자료: mq=12 ", '\n')
		cat("  YEAR: df<-mkdate_series(df, 2015)   ", '\n')
		cat("  Quarter: df<-mkdate_series(df, 2015, start_m=3/6/9/12, mq=4)   ", '\n')
		return( cat("  df<-mkdate_series(df, 시작연도:2015, 시작달:11, 월자료:12)   ", '\n') )  
		}

df<-as.data.frame(df)
n<-nrow(df)
tmp1<-rep(NA,n)
tmp2<-rep(NA,n)
tmp3<-rep(NA,n)

## monthly data START ----------------
if(start_m>0 & mq==12) { 

for(i in 1:n){
	if(start_m==12) 
		{tmp2[i]<-start_m
		 start_m<-1 
		 tmp1[i]<-start_y
		 start_y<-start_y+1}
	else{
		 tmp2[i]<-start_m
		 start_m<-start_m+1
		 tmp1[i]<-start_y
		} 
}

tmp1<-as.character(tmp1)
tmp2<-as.character(tmp2)

for (i in 1:n){
if(nchar(tmp2[i])==1) {
tmp2[i]<-paste0('0', tmp2[i], sep='')
} }
}

## monthly data END ----------------


# quarterly data START --------------------------

if(start_m>0 & mq==4) {
	for(i in 1:n){

if( (start_m != 3) & (start_m !=6 ) & (start_m != 9) & (start_m!=12) ) {
	cat(' ', '\n')
	cat('  Starting month should be one of 3, 6, 9, 12. ', '\n')
	cat(' ', '\n')
	break
}
		
		if(start_m==12) 
			{tmp2[i]<-start_m
			 start_m<-3
			 tmp1[i]<-start_y
			 start_y<-start_y+1}
		else{
			 tmp2[i]<-start_m
			 start_m<-start_m+3
			 tmp1[i]<-start_y
		} 
	}

	tmp1<-as.character(tmp1)
	tmp2<-as.character(tmp2)


	for (i in 1:n){
		if(nchar(tmp2[i])==1) {
		tmp2[i]<-paste0('0', tmp2[i], sep='')
		} }
	}

# quarterly data END --------------------------

if(start_m !=0) {
	for (i in 1:n){
		tmp3[i]<-paste0( tmp1[i], '-',tmp2[i], '-01', sep='')
		}

DATE<-as.Date(tmp3)
df<-cbind(df, DATE)
}

# yearly data START --------------------------
if(start_m==0) {
	for (i in 1:n){
	  tmp_start_y <- (start_y +i - 1)
	  tmp3[i]<-paste0( tmp_start_y,'-12-31', sep='')
	}
DATE<-as.Date(tmp3)
df<-cbind(df, DATE)
}
# yearly data END --------------------------

suppressPackageStartupMessages(library("dplyr"))
df <- df%>%relocate(DATE)

return(df)
}

