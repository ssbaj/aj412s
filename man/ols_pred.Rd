\name{ols_pred}
\alias{ols_pred}
\title{ols_pred}
\usage{
ols_pred()
}
\description{
Calculating probability from Logit Regression Results
}
\examples{

 예측치 계산1) 더미는 1증가, 연속변수 30%증가: ols_pred( OLSResult, 30)  

 예측치 계산2) 독립변수에 임의의 값 지정: my_input<-c(1,0,2)  *NOTE: 1=constant, 0='value of X1', 2='value of X2'  
 
 ols_pred( OLSResult, my_input)  

}