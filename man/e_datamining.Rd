\name{e_datamining}
\alias{e_datamining}
\title{e_datamining}
\usage{
e_datamining()
}
\description{
e_datamining Program
}
\examples{

## 파일로딩하기 -------
library(tidyverse)
df<-read.csv(file.choose(), skip=1, header=T, fileEncoding='CP949', encoding='UTF-8')

library(readxl)
df<-read_excel(file.choose(), skip=1)

## 변수를 선택해서 새 데이터셋으로 만들기 -------
인덱스 <- c(2,4,6)
새데이터셋 <- df[ , 인덱스]

## 자료를 솎아내서 새데이터셋 만들기 -------
새데이터셋 <- df[ df$debtratio !=Inf , ]
새데이터셋 <- df[ !is.na(df$debtratio) , ]
새데이터셋 <- df[ df$debtratio <= 124 , ]
새데이터셋 <- df[ complete.cases(df$편의성, df$가격만족, df$연령대), ] 

## 코드값 재코딩 -------
df$편의성2[df$conv==5] <- 4
df$성별2[df$gender==2] <- 0
df[df==''] <- NA 

## 자료수 확인 -------
sum(is.na(df$브랜드)) 
sum(!is.na(Adata$브랜드)) 
sum(Adata$브랜드=='Ford')

## 변수명 변경 -------
colnames(BasicData)<-c('id','brand','conv','satprice','age','gender')
colnames(BasicData)[6]<-c('성별')

## 더미변수 만들기 -------
library(fastDummies) 
df<-data(BasicData)
df12<-dummy_cols(df12, 'age')

## Grouping변수 만들기 -------
cn<-quantile(df$income, c(.25, .5, .75) , na.rm=T)
df$income2<-mkgroup(df$income, cn)

## 간단한 density그래프 그릭 -------
ggplot(df, aes(x=income)) + geom_density()

## 결과의 저장 -------
sink('결과저장.csv') 
  …… 
sink() 

## dplyr사용법 -------

df %>% group_by(브랜드) 
      %>% summarize(편의성평균=mean(편의성, na.rm=T), 표준편차=sd(편의성, na.rm=T), 자료수=n()) 

df %>% select(브랜드, 편의성)

df %>% filter(변수E==2015 & (변수F=='C' | 변수F=='G') )

df %>% arrange(연령대)

df <- rename(df, 새변수명=편의성)
또는, colnames(df)[1]<-c('피조사자이름')

merge(data0, data1, by='index')
   %>% merge(data2, by='index')
   %>% merge(data3, by='index')
   
## xtabs사용법 --------------
xtabs를 사용해 막대그래프 그리기
xtabs(~df$성별+df$브랜드) 
barplot(xtabs(~df$성별+df$브랜드), beside=T, horiz=F, ylab='빈도수') 

## rowSums 및 rowMeans ---------
rowSums 및 rowMeans 명령문 
Adata$check<-rowSums(Adata[, c(3:4)], na.rm=T) 
Adata$check<-rowMeans(Adata[, c(3:4)], na.rm=T) 


}