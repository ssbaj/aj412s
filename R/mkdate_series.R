mkdate_series<-function(df, start_y, start_m, mq ){

if (base::missing(df)) {
	    cat("  df<-as.data.frame(df) ", '\n')
		cat("  Input date format: 연도=2015, 시작 월=5, 월별자료=12 or 분기별자료=4  ", '\n')
		return(cat("  df<-mkdate_series(df, 2015, 5, 12)   ") )  }

df<-as.data.frame(df)
n<-nrow(df)
tmp1<-rep(NA,n)
tmp2<-rep(NA,n)
tmp3<-rep(NA,n)

if(mq==12) {    ## monthly data
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
} }

tmp1<-as.character(tmp1)
tmp2<-as.character(tmp2)

for (i in 1:n){
if(nchar(tmp2[i])==1) {
tmp2[i]<-paste0('0', tmp2[i], sep='')
} }
}


if(mq==4) {  #quarterly data
for(i in 1:n){
if(start_m==10) 
{tmp2[i]<-start_m
start_m<-1
tmp1[i]<-start_y
start_y<-start_y+1}
else{
tmp2[i]<-start_m
start_m<-start_m+3
tmp1[i]<-start_y
} }

tmp1<-as.character(tmp1)
tmp2<-as.character(tmp2)

for (i in 1:n){
if(nchar(tmp2[i])==1) {
tmp2[i]<-paste0('0', tmp2[i], sep='')
} }
}


for (i in 1:n){
tmp3[i]<-paste0( tmp1[i], '-',tmp2[i], '-01', sep='')
}

DATE<-as.Date(tmp3)

df<-cbind(df, DATE)
return(df)

}


