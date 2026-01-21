\name{geodistance22}
\alias{geodistance22}
\title{Calculating distance between two points}
\usage{
geodistance22()
}
\description{
Calculating distance between two points
}
\examples{

geodistance22()
 # llibrary(aj412s2); library(geosphere); library(dplyr)  
 # 위도(lat)와 경도(long)로 CENTER와 TARGET의 거리 측정 ---  
 # CENTER 데이터셋과 TARGET 데이터셋에 각각 lat와 long 변수가 있어야 함 ---

 geodistance22(CENTER, TARGET)
  --- 스크롤에 거리가 기록됩니다
  --- tmp<-geodistance22(CENTER, TARGET) # 결과를 tmp 데이터셋에 저장 ---
}
