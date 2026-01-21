\name{e_merge}
\alias{e_merge}
\title{merging dataset}
\usage{
e_merge()
}
\description{
Merging two datasets keeping common id variables
}
\examples{

# 두 데이터셋을 연결하는 변수는 id
# 두 데이터셋을 연결하는 변수가 공통으로 존재해야 함

# 데이터셋-1
> df1
  id age gender
1  1  23      1
2  2  45      2
3  3  32      1
4  4  NA      1
5  5  56      2

# 데이터셋-2
> df2
  id income
1  1      2
2  2      3
3  3      4
4  4      3
5  5      5
> merge( df1, df2, all=F )
  id age gender income
1  1  23      1      2
2  2  45      2      3
3  3  32      1      4
4  4  NA      1      3

}
