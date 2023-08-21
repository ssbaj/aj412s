\name{e_ttest2}
\alias{e_ttest2}
\title{Calc: t-test of two group means}
\usage{
e_ttest2()
}
\description{
Calculating t-test of two group means
}
\examples{

library(jmv); library(readxl)  
Adata<-read_excel('foot.xlsx')  
jmv::ttestIS(   
formula = footsize ~ gender,  # 양측검정  
data = Adata, # 데이터프레임명을 Adata로 지정   
vars = footsize,   
welchs = TRUE,   
# norm = TRUE,  # normality 검정/Shapiro-Wilk검정   
eqv = TRUE,   # Levene’s Test   
desc = TRUE ) 
}