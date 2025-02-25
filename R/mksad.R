## making seasonal adjusted data(mksad)

mksad<-function(df,  DATE_col,  data_col,  SeaMethod=1 , ma.method=5) {

 if (base::missing(df)) {
	    cat("  # Install.packages(c('dplyr', 'seasonal', 'forecast'))  ", '\n')
        cat("  방법1) x11으로 계절조정: df<-mksad(df, DATE컬럼번호, data컬럼번호) ", '\n') 
        cat("  방법2) 이동평균법으로 seasonal adjust  옵션 지정 ----------  ", '\n') 
        cat("        5기간 이동평균: df<-mksad(df, DATE컬럼번호, data컬럼번호,  2  ) ", '\n')
        cat("        5기간 외의 이동평균: df<-mksad(df, DATE컬럼번호, data컬럼번호,  2 , ma.method=이동평균_기간/디폴트_5기간 ) ", '\n')
        cat("            (1) adjusting.method의 디폴트는 1(=x11방법), 2(=이동평균법) ", '\n')
return(cat("            (2) 이동평균법 사용시, 3기간으로 하려면 ma.method=3 / 7기간은 ma.method=7 등등)", '\n'))
 }

library(dplyr)
df<-as.data.frame(df)

if( class(df[ ,DATE_col])!="Date")  {
cat('   2번째 column의 데이터 형태가 Date가 아닙니다', '\n')
break }

month_count<-NA
month_type<-NA
year_start<-NA

nrow_df <- nrow(df)
tmp_month<-rep(NA, nrow_df )

for(i in 1:nrow_df ){
tmp_month[i]<-substring(df[i, DATE_col], 6,7)
}

month_type<-sort(unique(tmp_month))
if(length(month_type)>4) { FREQ=12 } else { FREQ=4 }

year_start<-as.numeric(substring(df[1, DATE_col], 1,4))
month_start<-as.numeric(substring(df[1, DATE_col], 6,7))

target_variable<-ts(df[,data_col], start=c(year_start, month_start), freq=FREQ)  # 특정 data를 지정한다

if(SeaMethod==1){
fit_target_variable <- target_variable%>%seasonal::seas(x11='') 
x11_sa <- forecast::seasadj(fit_target_variable) 
df<-cbind(df, x11_sa)
df$x11_sa<-ts( df$x11_sa, start=c(year_start, month_start), freq=FREQ)  # 특정 data를 지정한다
return(df)
}

if(SeaMethod == 2){
# 방법2: data_5mv()는 forecast의 ma()와 동일한 명령문
#  library(forecast) 
#  ma(df[,'gdp'], 4)      
ma.method<-ma.method-1
ma5_sa <- forecast::ma(target_variable, ma.method)
df<-cbind(df, ma5_sa)
return(df)
}

}