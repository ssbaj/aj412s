\name{openxlsx}
\alias{openxlsx}
\title{Loading excel files}
\usage{
openxlsx()
}
\description{
Loading excel files
}

\examples{

  # file에 변수명이 포함되었으면 header=T / 변수명이 없으면 따로 지정할 필요 없음  
  # file에 1번~8번행이 설명문일 때, 설명문을 제거한 후 데이터를 로딩하려면 skip=8  
 
  df<-openxlsx( 'DATA21.xlsx', header=T, skip=0 ) 
  df<-openxlsx( 'DATA21.xlsx', header=T) 
  df<-openxlsx( 'DATA21.xlsx') # 디폴트: header=F, skip=0
}
