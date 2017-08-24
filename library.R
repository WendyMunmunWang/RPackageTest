#packages we need for building a package
install.packages("devtools")
devtools::install_github("klutometis/roxygen")
library("devtools")
library(roxygen2)

#create package directory
setwd("parent_directory")
create("cats")

cat_function <- function(){
  library("eRm")
  res.rasch <- RM(raschdat1)
  pres.rasch <- person.parameter(res.rasch)
  return(raschdat1)
}

setwd("./cats")
document()

#source: https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
