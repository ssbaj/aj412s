\name{e_APTdata}
\alias{e_APTdata}
\title{Modify Real Estate data for analysis}
\usage{
e_APTdata()
}
\description{
e_APTdata 부동산 실거래 공개시스템 데이터
}
\examples{
## 부동산 실거래 공개시스템(http://rt.molit.go.kr)
library(readxl)
library(dplyr)

# Adata<-read_excel('APT수지구TEST.xlsx')

a17<-read_excel('APT매매경기도2017.xlsx', skip=15)
a17<-a17[,c(1:11)]
a18<-read_excel('APT매매경기도2018.xlsx', skip=15)
a18<-a18[,c(1:11)]
a19<-read_excel('APT매매경기도2019.xlsx', skip=15)
a19<-a19[,c(1:11)]
a20<-read_excel('APT매매경기도2020.xlsx', skip=15)
a20<-a20[,c(1:11)]
a21<-read_excel('APT매매경기도2021.xlsx', skip=15)
a21<-a21[,c(1:11)]
a22<-read_excel('APT매매경기도2022.xlsx', skip=15)
a22<-a22[,c(1:11)]

Adata<-rbind(a17, a18, a19, a20, a21, a22)
Adata<-as.data.frame(Adata)
rm(a17, a18, a19, a20, a21, a22)
Adata<-Adata[,c(1, 5:11)]


# colnames(Adata)
# [1] "시군구"     "단지명"     "전용면적"   "계약년월"   "계약일"     "거래금액"  
# [7] "층"         "건축연도"   "year"       "평형"       "평당매매가"
 
# colnames(Adata)
# [1] "시군구"         "단지명"         "전용면적(㎡)"   "계약년월"      
# [5] "계약일"         "거래금액(만원)" "층"       


colnames(Adata)<-c('시군구','단지명','전용면적','계약년월','계약일','거래금액','층', '건축연도' )
Adata$평형<- round(Adata$전용면적/3.3, 0)
Adata$평당매매가<-round(Adata$거래금액/(Adata$전용면적/3.3), 0)

Bdata <-Adata%>%filter( grepl("상현동", 시군구) & grepl("광교상록자이", 단지명) )
Bdata<-as.data.frame(Bdata)
Cdata<-Bdata%>%group_by(평형, 계약년월)%>%summarize( 평당매매가평균=mean(평당매매가), 거래횟수=n())

##################################################
temp1<-sort(unique(Adata$계약년월))
count0<-length(temp1)

# 시작 YearMonth
temp1[1]

# 마지막 YearMonth
temp1[count0]

# temp1[1]을 그대로 입력
# Start YearMonth, (예:201701) 
ans0 <- "201701"

# temp1[1]을 참조해 Day를 01로 지정
# Start Year-Month-Day, (예:2017-01-01)
ans1<-"2017-01-01"

# temp1[count0]을 참조해 Day를 30 또는, 31로 지정
# End Year-Month-Day, (예:2021-12-31)
ans2<- "2021-12-31"


elapsed_months <- function(start_date, end_date) {
    ed <- as.POSIXlt(end_date)
    sd <- as.POSIXlt(start_date)
    12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}


CNT <-elapsed_months(ans1, ans2) + 1
ans0<-as.numeric(ans0)
year0 <-as.numeric( substring(ans0, 1, 4) )
month0 <-as.numeric( substring(ans0, 5, 6) )

Bdata<-as.data.frame( matrix(NA, nrow=CNT) )
Bdata$V1<-NA; Bdata$V2<-NA; Bdata$V3<-NA; Bdata$V4<-NA; Bdata$V5<-NA; 
Bdata$V6<-NA; Bdata$V7<-NA; Bdata$V8<-NA; Bdata$V9<-NA

r=c()

for(i in 1:CNT){
month1<-month0
if(month1<10) {month1<-paste0('0', month1)}
else {month1<-as.character(month1)}
TempDate<-as.numeric(paste0(year0, month1))
r=c(r, TempDate)
if(month0==12) {year0 <- year0 + 1
month0<-0}
month0<-month0+1 }

timeindex <- r

##############################################################################
colnames(Cdata)<-c('평형','계약년월','평당매매가평균', '거래횟수')
xtabs(~Cdata$평형)
평형index<-as.numeric(names(xtabs(~Cdata$평형)))
평형인덱스<-평형index

TempCount <- length(평형index)

Temp<-as.data.frame(matrix(data=rep(c(NA),(TempCount*CNT)*4), ncol=4))
colnames(Temp)<-c('평형','계약년월','평당매매가평균', '거래횟수')

for(kM in 1:TempCount){
alpha <- kM*CNT-(CNT-1)
beta <- kM*CNT
for(iM  in  alpha: beta ){
Temp$평형[iM] <- 평형index[kM]
}}

for(kM in 1:TempCount){
alpha <- kM*CNT-(CNT-1)
beta <- kM*CNT
for(iM  in  alpha: beta ){
Temp$계약년월[iM] <- timeindex[iM-(kM-1)*CNT]
}}


# for(kM in 1:TempCount){
# alpha <- kM*CNT-(CNT-1)
# beta <- kM*CNT
# for(iM  in  alpha: beta ){
# Temp$평형[iM] <- 평형index[kM]
# }}

# for(kM in 1:TempCount){
# alpha <- kM*CNT-(CNT-1)
# beta <- kM*CNT
# for(iM  in  alpha: beta ){
# Temp$계약년월[iM] <- timeindex[iM-(kM-1)*CNT]
# }}


TempCount2 <- (TempCount * CNT )

for( j in 1:TempCount2) {
for( i in 1:nrow(Cdata) ) {
if(Temp$평형[j]==Cdata$평형[i]  & Temp$계약년월[j]==Cdata$계약년월[i] )
{Temp$평당매매가평균[j] <- Cdata$평당매매가평균[i] 
 Temp$거래횟수[j] <- Cdata$거래횟수[i] } }}

평형인덱스

##################################################################
## 최초의 데이터가 NA일 때를 고려한 조치방안
## Temp 데이터셋의 자료를 보고 각 평형대가 시작하는 최초의 데이터가 NA이면
## 가장 가까운 값을 가져와서 최초의 데이터값을 채워 넣는다.
## 현대성우3차의 경우는 다음과 같다.
## Temp[,3][1]<-Temp[,3][5]
## Temp[,3][61]<-Temp[,3][63]
## Temp[,3][121]<-Temp[,3][135]

## NA이면 이전 데이터값으로 채워넣기
Temp[,3]<-zoo::na.locf(Temp[,3])

colnames(Temp)<-c('pyung','contday','price', 'freqTrading')
Temp$contyear<-substr(Temp$contday, 1,4)
Temp$contmonth<-substr(Temp$contday, 5,6)
Temp$contdate0<-paste0(Temp$contyear, '-',Temp$contmonth, '-01')
Temp$contdate0<-as.Date(Temp$contdate0)

### 수정해야 할 부분: pyung ==
plot( subset( Temp,  pyung==31, select = c( contdate0, price ) ) , type='l', ylim=c(500, 2600))
par(new=T)
plot( subset( Temp,  pyung==42, select = c( contdate0, price ) ) , type='l', ylim=c(500, 2600))
par(new=T)
plot( subset( Temp,  pyung==56, select = c( contdate0, price ) ) , type='l', ylim=c(500, 2600), col='red', xlab='', ylab='' )

}