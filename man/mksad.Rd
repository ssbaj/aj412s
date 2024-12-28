\name{mksad}
\alias{mksad}
\title{Making Seasonal Adjusted Data}
\usage{
mksad()
}
\description{
making seasonal adjusted data
}
\examples{

mksad()

# Install.packages(c('magrittr', 'seasonal', 'forecast'))   

방법1) x11으로 계절조정: df<-mk_sa(df, DATE컬럼번호, Data컬럼번호)  

방법2) 이동평균법으로 seasonal adjust  옵션 지정 ----------   
      5기간 이동평균: df<-mk_sa(df, DATE변수명, data변수명,  2  )
      7기간 이동평균: df<-mk_sa(df, DATE변수명, data변수명,  2 , ma.method=7 )
      3기간 이동평균: df<-mk_sa(df, DATE변수명, data변수명,  2 , ma.method=3 )
          (1) adjusting.method의 디폴트는 1(=x11방법), 2(=5기간 이동평균법)
          (2) 이동평균법 사용시, 3기간으로 하려면 ma.method=3 / 7기간은 ma.method=7 등등) 


}
