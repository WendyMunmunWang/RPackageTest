#packages we need for building a package
packages <- c("devtools", "eRm")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
devtools::install_github("klutometis/roxygen")
library("devtools")
library(roxygen2)
library("eRm")
#create package directory
setwd("parent_directory")
create("astromERM")

fn_anova_llra <- function(){
  #call erm.fn
}

setwd("./cats")
document()

#source: https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
