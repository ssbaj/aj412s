logit <- function(formula, data=data) {
   cat("\033[1;34m       Original command: glm(formula, family = binomial(link='logit') , data=df)  \033[0m", '\n')
   cat("\033[1;34m       If you want to add the subset option, use Original command. \033[0m", '\n')
   tmp.command<-glm(formula, family = binomial(link='logit') , data=data)
return(tmp.command) }
