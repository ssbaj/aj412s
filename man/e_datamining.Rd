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

## csv 데이터파일 로딩하기 -----
  df<-read.csv('df55.csv')
  print(df, max=99999)

## 한글 변수에 문제가 있을 경우 -----
  library(dplyr)
  df<-read.csv('df55.csv', fileEncoding='CP949', encoding='UTF-8')

## csv 데이터파일 저장하기 -----
  write.csv(df, '아무거나.csv')

## Excel 데이터파일 로딩하기 -----
  library(readxl)
  df<-read_xlsx('df88.xlsx')

## Excel 데이터파일 저장하기 -----
  library(writexl)
  write_xlsx(df, '아무거나.xlsx')

## colnames() : 변수명 바꾸기 -----
  df<-data.frame(   
   V1=c(1,2,3,1,2,3),   
   V2=c(23,45,32,NA,56,76),   
   V3=c(1,NA,1,1,2,1))   
   colnames(df)<-c('index', 'age', 'gender')

## complete.cases() : NA미싱데이터 삭제하기 ----
  df<-df[complete.cases(df), ]
  df<-df[complete.cases(df$age), ]

## 변수를 선택해서 새 데이터셋으로 만들기 ----
  index <- c(2,4,6)
  새데이터셋 <- df[ , index]

## 변수의 속성을 한꺼번에 변경하기 -------
  index <- c(2,4,6)
  New_df <- lapply(df[, index], as.character)

## NA미싱데이터 자료수 확인1 -------
  sum(is.na(df$age))

## NA미싱데이터 자료수 확인2 -------
  library(aj412s)
  na_col(df)

## 변수명 변경 -------
  library(aj412s)
  colnames(BasicData)<-c('id','brand','conv','satprice','age','gender')
  colnames(BasicData)[6]<-c('성별')

## index변수 만들기 -------
  df <- mkindex(df)

## 더미변수 만들기 -------

# 방법1
  library(aj412s)
  df<-BasicData
  
  unique(df$conv) 
  # conv의 변수값: 3 NA  2  1  4  5

  df$dum1<-0; df$dum2<-0; df$dum3<-0; df$dum4<-0; df$dum5<-0
  for(i in 1:n) {
      if( is.na(df$conv[i]) ) { next }
      else if( df$conv[i] == 1 ) {df$dum1[i]<-1}
      else if( df$conv[i] == 2 ) {df$dum2[i]<-1}
      else if( df$conv[i] == 3 ) {df$dum3[i]<-1}
      else if( df$conv[i] == 4 ) {df$dum4[i]<-1}
      else if( df$conv[i] == 5 ) {df$dum5[i]<-1}
   }

# 방법2
  library(aj412s)
  df<-BasicData
  df<-df[complete.cases(df$conv), ]
  df<-mkdum22(df, conv)

## Grouping변수 만들기 -------
  cn<-quantile(df$expend, c(.25, .5, .75) , na.rm=T)
  df<-mkgroup22(df, expend, cn)

## 결과의 저장 -------
  sink('결과저장.txt') 
    …… 
  sink() 

# aggegate vs. group_by() summarize()
  aggregate(가격만족도 ~ 브랜드, data = 데이터셋, FUN = mean)
  데이터셋 %>% group_by(브랜드) %>% summarize(mean(가격만족도, na.rm=T))

## table사용법 --------------
  table를 사용해 막대그래프 그리기
  table(df$성별, df$브랜드) 
  barplot(table(df$성별, df$브랜드), beside=T, horiz=F, ylab='빈도수') 

## rowSums 및 rowMeans ---------
  Adata$check<-rowSums(Adata[, c(3:4)], na.rm=T) 
  Adata$check<-rowMeans(Adata[, c(3:4)], na.rm=T) 

## dplyr의 filter명령문 대신 자료를 솎아내서 새데이터셋 만들기 -------
  df2 <- df[ df$debtratio !=Inf ,  ]
  df2 <- df[ !is.na(df$debtratio) ,  ]
  df2 <- df[ df$debtratio <= 124 ,  ]
  df2 <- df[ complete.cases(df) ,  ] 
  df2 <- df[ complete.cases(df$편의성, df$가격만족, df$연령대) ,  ] 

## 코드값 재코딩 -------
  df$편의성2[df$conv==5] <- 4
  df$성별2[df$gender==2] <- 0
  df[df==''] <- NA 

## dplyr사용법 -------

  df %>% group_by(브랜드) 
      %>% summarize(편의성평균=mean(편의성, na.rm=T), 표준편차=sd(편의성, na.rm=T), 자료수=n()) 

  df %>% select(브랜드, 편의성)

  df %>% filter(변수E==2015 & (변수F=='C' | 변수F=='G') )

  df %>% arrange(연령대, 성별)  또는, df %>% arrange(desc(연령대), 성별)

  df <- rename22(df, 새변수명=편의성)
  또는, colnames(df)[1]<-c('새변수명')

  merge(data1, data2, by='id')
     %>% merge(data3, by='id')
   
  merge(data1, select(data2, -c(v3) ) , by='id')
     %>% merge(data3, by='index')

  df <- BasicData %>% mutate(gm.satprice = satprice-mean(satprice, na.rm=T), 
                             gm.conv = conv-mean(conv, na.rm=T))

  df <- data.frame(category = c("A", "B", "A", "C", "B"))
  df %>% mutate(category = recode(category, "A" = "Apple", "B" = "Banana", "C" = "Cherry"))


}