\name{find_it}
\alias{find_it}
\title{find_it}
\usage{
find_it()
}
\description{
find related values
}
\examples{

# df에서 원하는 문자, 숫자, NA 찾기

 # 명령문: find_it(데이터셋, 변수값, 변수명)   
 # 명령문: 변수값이 문자이면 따옴표로 감싸고, 숫자이면 그대로 입력   
   find_it(df, 'David', Name) 
   find_it(df, 36, Age) 
   find_it(df, NA, ID) 

}
