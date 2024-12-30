\name{e_aggregate}
\alias{e_aggregate}
\title{aggregate dataset}
\usage{
e_aggregate()
}
\description{
aggregate variable
}
\examples{

## aggregate 사용법 --------  
  # aggregate : 종속변수의 값을 독립변수 값에 따라 집계한 후,  
                평균mean, 자료수length, 중위수median 등의 함수 값을 출력한다 
                사용예: aggregate(종속변수~독립변수, 데이터셋, 함수mean) 
     
  df<-data.frame(   
   area=c(1,2,3,1,2,3),   
   age=c(23,45,32,21,56,76),   
   gender=c(1,2,1,1,2,1))   
     
   aggregate(age~area, df, mean)  
     area  age  
   1    1 22.0  
   2    2 50.5  
   3    3 54.0  

}
