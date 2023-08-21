\name{mykosis}
\alias{mykosis}
\title{rearrange kosis data}
\usage{
mykosis()
}
\description{
rearrange kosis data
}
\examples{

# Adata의 변수는 3개: 1번=지역 소방서, 2번 변수=화재원인, 3번 변수=데이터
# 데이터 : https://data.seoul.go.kr/dataList/10949/S/2/datasetView.do
# 원본자료: Data45_KOSIS.xlsx

  library(readxl); library(aj412s)
  df <- read_excel('Data45_KOSIS_industry.xlsx')

# V1:지역명(예, 전국, 서울), V2:변수명(예, 전체사업, 교육서비스), V3:사업체수, V4:종사자수
# 원자료에서 V1(전국, 서울 등)이 NA로 표기된 것은 앞의 레코드로 채워넣는다
# V1, V2, V3변수만을 사용해 지역별, 사업별, 사업체수를 테이블로 만들기
# V1, V2, V4변수만을 사용해 지역별, 사업별, 종사자수를 테이블로 만들기

  df$V1<-zoo::na.locf(df$V1)
  df<-df[,c(1:3)]
  NewData<-mykosis(df)

}