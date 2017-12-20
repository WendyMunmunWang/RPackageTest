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

fn_anova_eRm <- function(object, ...){
  return(anova.eRm(object, ...))
}

fn_anova_llra <- function(object){
  return(anova(object))
}

fn_build_W <- function(X, nitems, npoints, grp_n, groupvec, itmgrps){
  return(build_W(X, nitems, npoints, grp_n, groupvec, itmgrps))
}

fn_cldeviance <- function(object, groups.gr = "rawscore", pi.hat){
  return(cldeviance(object, groups.gr = "rawscore", pi.hat))
}

fn_cmlprep <- function(X01,mt_vek,mpoints,Groups,W,gmemb){
  return(cmlprep(X01,mt_vek,mpoints,Groups,W,gmemb))
}

fn_coef.eRm <- function(object, parm = "beta", ...) {
  return(coef.eRm(object, parm="beta", ...))
}

fn_coefppar <- function(object, extrapolated = TRUE, ...) {
  return(coef.ppar(object, extrapolated = TRUE, ...))
}

fn_collapse_W <- function(W, listItems, newNames){
  return(collapse_W(W, listItems, newNames))
}

fn_confint_eRm <- function(object, parm="beta", level = 0.95, ...){
  return(confint.eRm(object, parm="beta", level = 0.95, ...))
}

fn_confint_ppar <- function(object, parm, level = 0.95, ...){
  return(confint.ppar(object, parm, level = 0.95, ...))
}

fn_gofIRT <- function(object, groups.h1, cutpoint){
  return(gofIRT(object, groups.h1 , cutpoint))
}

fn_confint_threshold <- function(object, parm, level = 0.95, ...){
  return(confint.threshold(object, parm, level = 0.95, ...))
}

fn_cwdeviance <- function(object, pi.hat){
  return(cwdeviance(object, pi.hat))
}

fn_datcheck.LRtest <- function(x, X, model){
  return(datcheck.LRtest(x, X, model))
}


fn_IC <- function(object){
  return(IC(object))
}

#itemfit.ppar function
fn_itemfit_ppar <- function(object, x, visible){
  return(itemfit.ppar(object, x, visible))
}

#item_info function
fn_item_info <- function(ermobject, theta, hvec, itembeta){
  return(item_info(ermobject, theta, hvec, itembeta))
}

#LLRA function
fn_LLRA <- function(X, W, mpoints, groups, baseline, itmgrps, x){
  return(LLRA(X, W, mpoints, groups, baseline, itmgrps, x))
}

#llra.datprep function
fn_llra_datprep <- function(X, mpoints, groups, baseline){
  return(llra.datprep(X, mpoints, groups, baseline))
}


setwd("./cats")
document()

#source: https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
