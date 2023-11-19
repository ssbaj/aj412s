\name{opencsv}
\alias{opencsv}
\title{Loading csv file containing Korean}
\usage{
opencsv()
}
\description{
Loading csv file containing Korean
}

\examples{

  # file에 한글이 포함된 csv파일 로딩 명령문  
  # file에 변수명이 포함되었으면 header=T / 변수명이 없으면 따로 지정할 필요 없음  
  # file에 1번~8번행이 설명문일 때, 설명문을 제거한 후 데이터를 로딩하려면 skip=8  
 
  df<-opencsv( '가계금융복지2018.csv', header=F, skip=0 )  
  
}
