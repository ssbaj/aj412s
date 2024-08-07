\name{e_stat}
\alias{e_stat}
\title{Explaining basic statistical method}
\usage{
e_stat()
}
\description{
Explaining basic statistical method
}
\examples{

## Sampling -------  
  index<-sample(nrow(Adata), 6, replace=F)  
  Adata[index , ]  
    
## wald test ------    
  fit<-lm(inc~debt+house+spouse2, data=Adata)  
  linearHypothesis(fit, c('debt=3', 'spouse2+debt3<=10'))  
  linearHypothesis(fit, c('debt=spouse2'))  
    
## NA미싱데이터를 삭제하지 않은 자료에 NA가 있으면, na.rm=T 사용 -------    
  mean(df$inc, na.rm=T)   
  sd(df$inc, na.rm=T)  
  library(dplyr)  
  df%>%summarize(평균=mean(inc, na.rm=T))  

## 기술통계 with jmv -------   
  library(aj412s)  
  df<-BasicData  
  df$gender<-as.factor(df$gender)  
    
jmv::descriptives(   
  data = df,  
  vars = c(gender, inc, debt),  
  skew = FALSE,   
  kurt = FALSE,  
  freq = TRUE,  
  bar = TRUE,  
  dens = TRUE,  
  hist = TRUE,  
  violin = TRUE)  

## 회귀분석 formula -------  
  결과<-lm(소득~ . , data=Ada)   모든변수 선택  
  결과<-lm(소득~ . -1 - 학교소속감, data=Ada)   변수제외   
  결과<-lm(소득~ 학교소속감:성별, data=Ada)   상호작용항   
  결과<-lm(소득~ 학교소속감 * 성별, data=Ada)   모든변수 + 모든상호작용   
  결과<-lm(소득~ I(학업탈진+성별-10), data=Ada)   괄호내 수식에 따라 변수 생성   
    
##확률 및 p값 --------  
  t분포 경계값: qt(확률, n-1)   
  t분포 p값찾기: 1-pt(t검정통계량, n-1)   
  카이자승 경계값: qchisq(확률, 자유도)   
  카이자승 p값찾기: 1-pchisq(카이자승_검정통계량, 자유도)   
  F분포 경계값: qf(확률, 자유도1, 자유도2)   
  F분포 p값찾기: 1-pf(F검정통계량, k-1, n-k)   
  정규분포 확률 : pnorm(z검정통계량)   
  정규분포 경계값: qnorm(확률)   
  정규분포 p값찾기: 1-pnorm(z검정통계량)   

}