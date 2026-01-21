\name{Mpower}
\alias{Mpower}
\title{Title: multiplying a matrix}
\usage{
Mposer()
}
\description{
Description: 상태전환표를 행방향 확률형태로 만든 후, n기간 후의 상태를 예측하기 위한 명령문
}
\examples{

# 예제: 확률형태 상태전환표 만들기
Adata<-matrix(c(0.7, 0.3, 0.75, 0.25), nrow=2, ncol=2, byrow=TRUE)

# 3기간 후의 상태 계산하기
#> Mpower(Adata, 3)
#        [,1]      [,2]
# [1,] 0.714250  0.285750
# [2,] 0.714375  0.285625

}






