\name{probit_prob}
\alias{probit_prob}
\title{probit_prob}
\usage{
probit_prob()
}
\description{
Calculating probability from probit
}
\examples{

 확률계산1) 더미는 1증가, 연속변수 30%증가: probit_prob( ProbitResult, 30)  

 
 확률계산2) 독립변수에 임의의 값 지정: my_input<-c(1,0,2)  *NOTE: 1=constant, 0='value of X1', 2='value of X2'  
 
 probit_prob( ProbitResult, my_input)  

}