probit <- function(name_command, data=data) {
   tmp.command<-glm(name_command, family = binomial(link='probit') , data=data)
return(tmp.command) }
