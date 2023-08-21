\name{e_logit}
\alias{e_logit}
\title{Calc: logit estimation}
\usage{
e_logit()
}
\description{
Logit model
}
\examples{
library(gmodels); library(stargazer); library(readxl)  
Adata<-read.csv('Data20_Mroz.csv', header=T)  

---- character형로 기록된 wc변수와 hc변수값을 numeric형태로 전환 ---- 
Adata$wc2<-c2n(Adata$wc)  
Adata$hc2<-c2n(Adata$hc)  

------------ logit분석하기 ------------   
아무거나1 <- glm(lfp~k5+k618+age+wc2+hc2+lwg+inc, family='binomial', data=Adata)  

--------- 시뮬레이션: 첫 번째 입력값은 1(상수항) ---------  
아무거나2<-c(1, 0, 1, 43, 0, 0, median(Adata$lwg), median(Adata$inc))  
아무거나3<-coef(아무거나1)  
아무거나4<-sum(아무거나3*아무거나2)  
Result_exp=exp(아무거나4)  

------------ 확률계산하기 ------------   
logit_probability=Result_exp/(1+Result_exp)  
    
------------ 확률계산하기 ------------   
prob.블루 <- 1/(1+exp(y2)+exp(y3))   
prob.화이트 <- exp(y2)/(1+exp(y2)+exp(y3))   
prob.전문직 <- exp(y3)/(1+exp(y2)+exp(y3))   
}