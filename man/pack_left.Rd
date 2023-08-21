\name{pack_left}
\alias{pack_left}
\title{put together records without blank}
\usage{
pack_left()
}
\description{put together records without blank
}
\examples{

# 창업기업실태조사 -- 창업이유(V18~V27)
# V18~V27은 numeric으로 지정
# Bdata<-Adata[,c(18:27)]
# tmp<-pack_left(Bdata)
# tmp데이터셋에서 check변수 지우기
# 원본자료인 Adata(V1~V166) 및 tmp(index=V167, V168~V170) 병합하기
# Cdata<-merge(Adata, tmp, by='index')
# tmp2<-pack_down(Cdata, 170, 168)  #폐업원인3 -> 폐업원인1 레코드로 복사
# tmp3<-pack_down(tmp2, 169, 168)  #폐업원인2 -> 폐업원인1 레코드로 복사
# -------------------------------------------------------------

# 21년 창업기업실태조사 자료의 폐업1~7의 원인을 왼쪽 column부터 순차적으로 몰아주기
# V18~V27변수

> df<-read_excel('pack_data.xlsx')
> df<-as.data.frame(df)
> df<-pack_left(df)


# 임의의 예제 ---------------
> a1<-c(1,NA,3,4,NA)
> a2<-c(NA,17,19,NA,18)
> a3<-c(NA,NA,NA,49,0)
> Adata<-cbind(a1,a2,a3)
> Adata<-as.data.frame(Adata)
> Adata
  a1 a2 a3
1  1 NA NA
2 NA 17 NA
3  3 19 NA
4  4 NA 49
5 NA 18  0

> Adata<-as.data.frame(Adata)
> temp<-pack_left(Adata)
> temp
  a1 a2 a3 index check
1  1 NA NA     1     1
2 17 NA NA     2    17
3  3 19 NA     3    22
4  4 49 NA     4    53
5 18  0 NA     5    18


}