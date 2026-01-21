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

  my_input <- data.frame(  
  gender = rep('male', 61),  
  age = rep(30, 61),  
  income = seq(2000, 8000, 100)  
  )  

 predict(mlogit_Result, newdata = my_input, type = "probs")
 mlogit_prob(mlogit_Result, my_input)  

}
