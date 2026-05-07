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
library(plm)
panel_data <- pdata.frame(panel, index = c('area', 'year'))
diffDataSet<-panel_diff(panel_data)


}
