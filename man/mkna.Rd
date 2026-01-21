\name{mkna}
\alias{mkna}
\title{mkna manual}
\usage{
mkna()
}
\description{
How to make missing data by using mkNAs
}
\examples{

# 명령문

df1 <- mkna(Adata, 미싱데이터를 만들어야 하는 컬럼명,  미싱데이터 갯수)


# Adata의 5번째 컬럼에 해당되는 변수에 300개의 미싱데이터를 만들 경우

df1 <- mkna(df, satprice , 300)

}
