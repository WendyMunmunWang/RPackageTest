#download packages if you haven't yet
packages <- c("devtools", "eRm", "devtools")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
  devtools::install_github("klutometis/roxygen")
}

#include libraries
library("devtools")
library(roxygen2)
library("eRm")

#create package directory
setwd("parent_directory")
create("astromERM")

#anova.llra function
#Input params are object...
fn_anova_llra <- function(object){
  return(anova(object))
}

#build_W function
#Included all input params
fn_build_W <- function(X, nitems, npoints, grp_n, groupvec, itmgrps){
  return(build_W(X, nitems, npoints, grp_n, groupvec, itmgrps))
}

#collapse_W
#Included all input params
fn_collapse_W <- function(W, listItems, newNames){
  return(collapse_W(W, listItems, newNames))
}

#TODO: eRm.data

#gofIRT function
fn_gofIRT <- function(object, groups.h1, cutpoint){
  return(gofIRT(object, groups.h1 = groups.h1, cutpoint = cutpoint))
}

#IC function
fn_IC <- function(object){
  return(IC(object))
}

#itemfit.ppar function
fn_itemfit_ppar <- function(object, x, visible){
  return(itemfit.ppar(object, x, visible))
}


setwd("./cats")
document()

#source: https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
