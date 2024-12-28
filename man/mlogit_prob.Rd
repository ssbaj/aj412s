\name{mlogit_prob}
\alias{mlogit_prob}
\title{Caculate the probability of Mutinomial Logit}
\usage{
mlogit_prob()
}
\description{
mlogit_prob()
}
\examples{

# Examples ----------  

  new_data <- data.frame(  
  gender = rep('male', 61),  
  age = rep(30, 61),  
  income = seq(2000, 8000, 100)  
  )  
 
 mlogit_prob(Result, new_data)  
}
