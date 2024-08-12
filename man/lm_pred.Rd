\name{lm_pred}
\alias{lm_pred}
\title{lm_pred}
\usage{
lm_pred()
}
\description{
Calculating forecasted y values from OLS anlaysis
}
\examples{

 예측치 계산1) 더미는 1증가, 연속변수 20%증가: lm_pred( 분석결과)  
 
 예측치 계산2) 더미는 1증가, 연속변수 40%증가: lm_pred( 분석결과, 40)  

 예측치 계산3) 독립변수에 임의의 값 지정: my_input<-c(1,0,2)  *NOTE: 1=constant, 0='value of X1', 2='value of X2'  
 
               lm_pred( OLSResult, my_input) 

}