\name{opentxt}
\alias{opentxt}
\title{Loading Text file containing Korean}
\usage{
opencsv()
}
\description{
Loading txt file containing Korean
}

\examples{

  # file에 한글이 포함된 txt파일 로딩 명령문  
  # file에 변수명이 포함되었으면 header=T / 변수명이 없으면 따로 지정할 필요 없음  
 
  df<-opentxt( hsb1data.txt )
  df<-opentxt( hsb1data.txt, header=F, skip=0 )
  
}
