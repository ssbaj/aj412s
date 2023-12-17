# This is an example function named 'hello_git' 
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello_git <- function() {
  
  cat("  git add .   ", '\n')
  cat("  git commit -m  "first commit"   ", '\n')
  cat("  git push origin master   ", '\n')
  cat("  devtools::install_github('ssbaj/aj412s')   ", '\n')

}
