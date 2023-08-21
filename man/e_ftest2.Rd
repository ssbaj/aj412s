\name{e_ftest2}
\alias{e_ftest2}
\title{Calc: Two-Way Ftest}
\usage{
e_ftest2()
}
\description{
Calculating Two-Way Ftest
}
\examples{
library(jmv); library(readxl) 
  
# Two-Way ANOVA  
# with(데이터셋, interaction.plot(독립변수1, 독립변수2, 종속변수, fun = mean, main = 'Interaction Plot')) 
with(Adata, interaction.plot(  gender, circle, involve, fun = mean, main = 'Interaction Plot')) 
  
Adata<-read.csv('Data1_Exhaust.csv') 
Adata$gender<-as.factor(Adata$gender) 
Adata$circle<-as.factor(Adata$circle) 
jmv::ANOVA(formula = involve ~ gender + circle + gender:circle,   
   data = Adata,   
   effectSize = 'eta',   
   homo = TRUE,  #Levene’s Test를 실행하란 명령문   
   postHoc = ~ gender + circle + gender:circle,   
   postHocCorr='tukey')  # Tukey방법의 PostHoc Test   
  
**** 다른 방법으로 PostHoc Test를 하려면 ****  
--- Scheffe방법의 PostHoc Test ---  
postHocCorr = 'scheffe'  
--- Bonferroni방법의 PostHoc Test ---  
postHocCorr = 'bonf'   
--- Holm방법의 PostHoc Test --- 
postHocCorr = 'holm'   
}