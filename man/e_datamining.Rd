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
df<-opencsv('자료1.csv', header=T, skip=0)
df<-openxlsx('자료1.xlsx', header=T, sheet=1, skip=0)

library(tidyverse)
df<-read.csv(file.choose(), skip=1, header=T, fileEncoding='CP949', encoding='UTF-8')

library(readxl)
df<-read_excel(file.choose(), skip=1)

## NA미싱데이터 삭제하기 -------
  df<-df[complete.cases(df), ]
  df<-df[complete.cases(df$conv, df$satprice), ]


## 변수를 선택해서 새 데이터셋으로 만들기 -------
  index <- c(2,4,6)
  새데이터셋 <- df[ , index]

## 변수의 속성을 한꺼번에 변경하기 -------
  index <- c(2,4,6)
  New_df <- lapply(df[, index], as.character)

## 자료수 확인 -------
  sum(is.na(df$brand))
  sum(!is.na(Adata$brand)) 
  sum(Adata$브랜드=='Ford')
  xtabs(~df$brand)
  xtabs(~df$brand+df$gender)

## 변수명 변경 -------
  colnames(BasicData)<-c('id','brand','conv','satprice','age','gender')
  colnames(BasicData)[6]<-c('성별')

## index변수 만들기 -------
  df <- mkindex(df)

## 더미변수 만들기 -------
# 방법1
  df<-BasicData
  df<-mkdum(df, 성별변수의 컬럼 번호)

# 방법2
  df$dum1<-0; df$dum2<-0; df$dum3<-0
  for(i in 1:n) {
    if( is.na(df$satprice[i]) ) { next }
    else if( df$satprice[i] <=2 ) {df$dum1[i]<-1}
    else if( df$satprice[i] ==3 ) {df$dum2[i]<-1}
    else if( df$satprice[i] >=4 ) {df$dum3[i]<-1}
  }

# 방법3
  df$dum1<-0; df$dum2<-0; df$dum3<-0
  df$dum1<-ifelse(df$satprice<=2, 1, 0)
  df$dum2<-ifelse(df$satprice==3, 1, 0)
  df$dum3<-ifelse(df$satprice>=4, 1, 0)

## Grouping변수 만들기 -------
  cn<-quantile(df$income, c(.25, .5, .75) , na.rm=T)
  df$income2<-mkgroup(df$income, cn)

## 간단한 density그래프 그릭 -------
  ggplot(df, aes(x=income)) + geom_density()

## 결과의 저장 -------
  sink('결과저장.txt') 
    …… 
  sink() 

## dplyr사용법 -------

  df %>% group_by(브랜드) 
      %>% summarize(편의성평균=mean(편의성, na.rm=T), 표준편차=sd(편의성, na.rm=T), 자료수=n()) 

  df %>% select(브랜드, 편의성)

  df %>% filter(변수E==2015 & (변수F=='C' | 변수F=='G') )

  df %>% arrange(연령대, 성별)  또는, df %>% arrange(desc(연령대), 성별)

  df <- rename(df, 새변수명=편의성)
  또는, colnames(df)[1]<-c('새변수명')

  merge(data1, data2, by='id')
     %>% merge(data3, by='id')
   
  merge(data1, select(data2, -c(v3) ) , by='id')
     %>% merge(data3, by='index')

  df <- BasicData %>% mutate(gm.satprice = satprice-mean(satprice, na.rm=T), 
                             gm.conv = conv-mean(conv, na.rm=T))

  df <- data.frame(category = c("A", "B", "A", "C", "B"))
  df %>% mutate(category = recode(category, "A" = "Apple", "B" = "Banana", "C" = "Cherry"))

# aggegate vs. group_by() summarize()
  aggregate(가격만족도 ~ 브랜드, data = 데이터셋, FUN = mean)
  데이터셋 %>% group_by(브랜드) %>% summarize(mean(가격만족도, na.rm=T))

## xtabs사용법 --------------
  xtabs를 사용해 막대그래프 그리기
  xtabs(~df$성별+df$브랜드) 
  barplot(xtabs(~df$성별+df$브랜드), beside=T, horiz=F, ylab='빈도수') 

## rowSums 및 rowMeans ---------
  Adata$check<-rowSums(Adata[, c(3:4)], na.rm=T) 
  Adata$check<-rowMeans(Adata[, c(3:4)], na.rm=T) 

## dplyr의 filter명령문 대신 자료를 솎아내서 새데이터셋 만들기 -------
  새데이터셋 <- df[ df$debtratio !=Inf , ]
  새데이터셋 <- df[ !is.na(df$debtratio) , ]
  새데이터셋 <- df[ df$debtratio <= 124 , ]
  새데이터셋 <- df[ complete.cases(df$편의성, df$가격만족, df$연령대), ] 

## dplyr의 recode나 aj412s의 recoding대신 코드값 재코딩 -------
  df$편의성2[df$conv==5] <- 4
  df$성별2[df$gender==2] <- 0
  df[df==''] <- NA 


}