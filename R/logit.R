logit <- function(name_command, data=data) {
   tmp.command<-glm(name_command, family = binomial(link='logit') , data=data)
return(tmp.command) }
