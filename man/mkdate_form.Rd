\name{mkdate_form}
\alias{mkdate_form}
\title{making year-month-day date Dataset}
\usage{
mkdate_form()
}
\description{
making year-month-day date Dataset
}
\examples{

# making year-month-day date Dataset

# Input: 2023.4 or 2023-4 // Output: DATE_form=2023-04-01
  df<-mkdate_form(df, 4=column of YEAR.MONTH )
  df<-mkdate_form(df, 4)

}