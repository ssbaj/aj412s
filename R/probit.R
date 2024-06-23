probit <- function(name_command, data=data) {
   cat("\033[1;34m       Original command: glm(formula, family = binomial(link='probit') , data=df)  \033[0m", '\n')
   cat("\033[1;34m       If you want to add the subset option, please use Original command. \033[0m", '\n')
   tmp.command<-glm(name_command, family = binomial(link='probit') , data=data)
return(tmp.command) }
