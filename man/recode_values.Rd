\name{recode_values}
\alias{recode_values}
\title{Recoding numbers}
\usage{
recode_values()
}
\description{
Recoding numbers
}
\examples{

 df$var1 <- recode_values(df$var1, 1~1, 2~1, 3~1, 4~1, 5~2, 6~3, 7~4)
 # Recoding numbers

}