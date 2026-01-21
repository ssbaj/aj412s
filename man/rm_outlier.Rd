\name{rm_outlier}
\alias{rm_outlier}
\title{Remove outliers}
\usage{
rm_outlier(BasicData)
}
\description{
Remove outliers
}
\examples{

 df <- rm_outlier(BasicData, option=2)
 df <- rm_outlier(BasicData, 2)
 df_clean <- df[ rm_outlier(df$inc) & rm_outlier(df$asset), ]

}