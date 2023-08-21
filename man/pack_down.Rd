\name{pack_down}
\alias{pack_down}
\title{put together records by adding records}
\usage{
pack_left()
}
\description{put together records by pasting records in the downward direction
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


# down_pack 함수. 폐업원인 1, 2, 3 등으로 
# 여러개 있으면 해당 레코드를 복사해
# 데이터셋 아래에 붙여넣기해 폐업원인을 
# 추가로 기록하는 함수

# aj412s::pack_left()를 사용해 왼쪽으로 
# 레코드를 몰아준 데이터셋tmp
# 더이터셋temp에 다른 변수를 추가함(광고비,매출액)
> tmp
  a1 a2 a3 광고비 매출액
1  1 NA NA 52   76
2 17 NA NA 53   73
3  3 19 NA 54   75
4  4 49 NA 55   79
5 18  6 NA 56   78

# tmp의 2번째 컬럼자료를 1번째 컬럼에 붙여넣고 2번째 컬럼은 NA로 채우기
> tmp2 <- pack_down(tmp, 2, 1)
> tmp2
  a1 a2 a3 광고비 매출액
1  1 NA NA 52 	76
2 17 NA NA 53 	73
3  3 NA NA 54 	75
4  4 NA NA 55 	79
5 18 NA NA 56 	78
6 19 NA NA 54 	75
7 49 NA NA 55 	79
8  6 NA NA 56 	78


}