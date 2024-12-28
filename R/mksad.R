mksad<-function(df, DATE_col, data_col, SeaMethod=1 , ma.method=5) {
 
if (base::missing(df)) {
    cat("\033[1;31m # 시계열 자료의 계절조정 ------------- \033[0m ", '\n')
    cat("\033[1;32m # mkdate_series로 DATE변수 만들기 : df<-mkdate_series(df, 시작연도:2015, 시작달:11, 월자료:12) \033[0m ", '\n')
    cat("\033[1;32m # Quarterly 인지 Monthly 데이터인지는 DATE값을 보고 자동 판독함 \033[0m ", '\n')
    cat("\033[1;31m 방법1) x11으로 계절조정: df<-mksad(df, DATE변수명, data변수명) \033[0m ", '\n')
cat("\033[1;31m 방법2) 이동평균법으로 seasonal adjust 옵션 지정 하기 ---------- \033[0m ", '\n')
cat(" 5기간 이동평균: df<-mksad(df, DATE변수명, data변수명, 2 ) ", '\n')
cat(" 5기간 외의 이동평균: df<-mksad(df, DATE변수명, data변수명, 2 , ma.method=이동평균_기간/디폴트_5기간 ) ", '\n')
cat(" (1) adjusting.method의 디폴트는 1(=x11방법), 2(=이동평균법) ", '\n')
return(cat(" (2) 이동평균법 사용시, 3기간으로 하려면 ma.method=3 / 7기간은 ma.method=7 ) ", '\n'))
}


if (!require(dplyr)) {
install.packages("dplyr")
}


if (!require(seasonal)) {
install.packages("seasonal")
}


if (!require(forecast)) {
install.packages("forecast")
}


## 변수명에 홑따옴표를 붙여 문자로 바꾸는 명령문

data_col <- deparse(substitute(data_col))
DATE_col<-deparse(substitute(DATE_col))

##-----------------------------------
# 변수명을 컬럼 번호로 변경시키는 함수
##------------------------------------
find_col2<-function(DataSet, index_id ){
tmp_colnames<-colnames(DataSet)
n<-length(tmp_colnames) # DataSet의 총변수 갯수

for(i in 1:n){
if(index_id==tmp_colnames[i]) {return(as.numeric(i))}
}
}

##----------------------------------------
# find_col2()를 사용해 컬럼번호 찾기
if(is.numeric(DATE_col)==F) {DATE_col<-find_col2(df, DATE_col) }
##----------------------------------------


##----------------------------------------
# find_col2()를 사용해 컬럼번호 찾기
if(is.numeric(data_col)==F) {data_col<-find_col2(df, data_col) }
##----------------------------------------

library(dplyr)
df<-as.data.frame(df)

if( class(df[ ,DATE_col])!="Date") {
cat(' 2번째 column의 데이터 형태가 Date가 아닙니다', '\n')
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

target_variable<-ts(df[,data_col], start=c(year_start, month_start), freq=FREQ) # 특정 data를 지정한다

if(SeaMethod==1){
fit_target_variable <- target_variable%>%seasonal::seas(x11='')
x11_sa <- forecast::seasadj(fit_target_variable)
df<-cbind(df, x11_sa)
df$x11_sa<-ts( df$x11_sa, start=c(year_start, month_start), freq=FREQ) # 특정 data를 지정한다
return(df)
}


if(SeaMethod == 2){
# 방법2: data_5mv()는 forecast의 ma()와 동일한 명령문
# library(forecast)
cat('Period for moving average:', ma.method, '\n' )
ma_sa <- forecast::ma(target_variable, ma.method)
df<-cbind(df, ma_sa)
return(df)
}

}
