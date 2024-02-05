probit <- function(name_command, data=data) {
   cat("Original command: glm(formula, family = binomial(link='probit') , data=df)", '\n')
   cat("If you want to add the subset option, please use Original command.", '\n')
   tmp.command<-glm(name_command, family = binomial(link='probit') , data=data)
return(tmp.command) }
