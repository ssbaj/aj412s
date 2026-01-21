\name{e_ztable}
\alias{e_ztable}
\title{Making Tables}
\usage{
e_ztable()
}
\description{
Making Tables
}
\examples{
library(moonBook); library(ztable); library(readxl); library(aj412s)
options(ztable.type='viewer')

## 데이터 로딩 및 영문 변수명으로 변경
Adata <- read_excel('자료1.xlsx')
colnames(Adata) <- c('brand','conv','satprice','age','gender')

## 회귀분석/로짓분석 결과를 테이블로 만드는 방법
re <- lm(satprice~gender+conv, data=Adata)
ztable(re) # Rstudio의 viewer에서 복사 -> 엑셀 -> 한글/워드에 복사

## 연속형 데이터에 대한 기술

# aj412s::desc2all()은 numeric변수만 Input으로 허용함
# 첫 번째 변수인 'brand'가 문자형character 변수이므로 제외하고 Input으로 입력
re <- desc2all(Adata[, -c(1)]) 
ztable(re) # Rstudio의 viewer에서 복사 -> 엑셀 -> 한글/워드에 복사

## 범주형 데이터에 대한 기술
re<-mytable(gender~satprice, data=Adata)  # 복사해서 텍스트로 저장 - 엑셀 - 한글/워드에 복사
ztable(re)

# max.ylev는 더미변수가 최대 몇개의 숫자로 코딩되었는지 지정
# max.ylev를 넘어서면 연속형변수로 인식

re<-mytable(~gender+age+edu, data=Adata, max.ylev=8)  
ztable(re)

}