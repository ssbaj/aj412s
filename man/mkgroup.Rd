\name{mkgroup}
\alias{mkgroup}
\title{Making grouping index variables}
\usage{
mkgroup()
}
\description{
Making grouping index variables
}
\examples{

## 예제 데이터셋 만들기
  set.seed(12345)
  Adata <- as.data.frame( matrix( rnorm(40), ncol=1, byrow=F ) )
  colnames(Adata) <- c('edu')

# Adata$testdata값을 4개의 그룹으로 나눔
  cn <- quantile(Adata$edu, c(.25, .5, .75))
  
# 또는 직접 입력도 가능함
  cn <- c(-0.36, 0.33, 0.78 )
  Adata<-mkgroup(Adata, edu)

}
