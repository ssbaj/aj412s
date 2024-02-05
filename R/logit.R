logit <- function(formula, data=data) {
   cat("Original command: glm(formula, family = binomial(link='logit') , data)=df", '\n')
   cat("If you want to add the subset option, please use Original command.", '\n')
   tmp.command<-glm(formula, family = binomial(link='logit') , data=data)
return(tmp.command) }
