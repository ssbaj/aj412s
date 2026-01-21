\name{panel_diff}
\alias{panel_diff}
\title{panel_diff}
\usage{
panel_diff()
}
\description{
differencing panel dataset
}
\examples{

## Example Dataset
v1<-c(3,4,5,2018)
v2<-c(4,6,7,2019)
v3<-c(7,3,8,2020)
v4<-c(13,14,15,2018)
v5<-c(24,26,27,2019)
v6<-c(37,33,38,2020)
v7<-c(33,44,65,2018)
v8<-c(88,58,36,2019)
v9<-c(73,45,58,2020)

df<-rbind(v1, v2, v3, v4, v5, v6, v7, v8, v9)
df<-as.data.frame(df)
colnames(df)<-c("X1","X2","X3","Year")
rownames(df)<-c(1,2,3,4,5,6,7,8,9)

## differencing panel dataset, year=3YR

tmp<-panel_diff(df, 3)


}
