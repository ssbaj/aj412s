\name{reshaping}
\alias{reshaping}
\title{Reshaping data forms}
\usage{
reshaping()
}
\description{
Reshaping data
}
\examples{

예) df={V1, V2, V3, V4, V5, V6, V7, V8}    
    
   df2<-reshaping(df, 2, 3)
   df=데이터셋, 2=앞의 2개 변수는 공통변수, 
      나머지 변수는 두 개씩 묶어서 3개로 나눈 후, 
      세로로 묶음. 
   예) V1 V2 V3 V4 : V1 V2 V5 V6 : V1 V2 V7 V8 : 
       3개 데이터셋으로 만든 후 세로로 묶음 

df2<-reshaping(df, common columns, repeating numbers) 

}