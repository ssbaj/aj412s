\name{addcomma}
\alias{addcomma}
\title{Adding commas in strings}
\usage{
addcomma()
}
\description{
Adding commas in strings
}
\examples{
tmp<-"a b c 1 2 3"

tmp2<-addcomma(tmp)

print(tmp2)
[1] "a,b,c,1,2,3"

}
