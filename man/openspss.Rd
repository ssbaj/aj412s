\name{openspss}
\alias{openspss}
\title{Loading SPSS files}
\usage{
openspss()
}
\description{
Loading sav files
}

\examples{

  # SPSS의 sav파일을 로딩함
  # sav파일은 항상 header가 있기 때문에 header명령문 지정은 필요없음
  # file에 1번~8번행이 설명문일 때, 설명문을 제거한 후 데이터를 로딩하려면 skip=8  
 
  df<-opensav( 'Z0345.csv')  
  
}
