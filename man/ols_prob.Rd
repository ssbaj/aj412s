\name{ols_prob}
\alias{ols_prob}
\title{ols_prob}
\usage{
ols_prob()
}
\description{
Calculating probability from Logit Regression Results
}
\examples{

 예측치 계산1) 더미는 1증가, 연속변수 30%증가: ols_prob( OLSResult, 30)  

 예측치 계산2) 독립변수에 임의의 값 지정: my_input<-c(1,0,2)  *NOTE: 1=constant, 0='value of X1', 2='value of X2'  
 
 ols_prob( OLSResult, my_input)  

}