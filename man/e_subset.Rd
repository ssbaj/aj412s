\name{e_subset}
\alias{e_subset}
\title{How to use : subset}
\usage{
e_subset()
}
\description{
How to use : subset
}
\examples{

# subset 데이터 --------  
  subset(df, 평형==32, select=c(date0, 평당매매가평균))  
  subset(df, (brand=='KIA' | brand=='GM') ,select=c(1,2,3,4))  
  subset(df, grepl('KI', df$brand) ,select=c(1,2,3,4))  
    
# subset 그래프 --------  
  plot(subset(df, 평형==32, select=c(date0, 평당매매가평균)), type='l', col='cornflowerblue', lwd=2, ylim=c(1000, 2800))  
    
# subset 회귀분석 --------  
  결과 <- lm(거래금액~전용면적+층, data=df, subset=c(year==2021 & 평형==32)  )  

}