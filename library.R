#packages we need for building a package
install.packages("devtools")
devtools::install_github("klutometis/roxygen")
install.packages("ltm")
install.packages("eRm")
library("devtools")
library(roxygen2)
library("eRm")
library("ltm")
#create package directory
setwd("parent_directory")
create("cats")

exampleOne_function <- function(){
  res.rasch <- RM(raschdat1)
  pres.rasch <- person.parameter(res.rasch)
  plotPImap(res.rasch, sorted = TRUE)
  lrres.rasch <- LRtest(res.rasch, splitcr = "mean")
  return(lrres.rasch)
}

exampleTwo_function <- function(){
  res.lltm <- LLTM(lltmdat2, W)
  summary <- summary(res.lltm)
  return(summary)
}

exampleThree_function <- function(){
  data(pcmdat2)
  res.rsm <- RSM(pcmdat2)
  thresholds(res.rsm)
  plotICC(res.rsm, mplot = TRUE, legpos = FALSE, ask = FALSE)
  
  res.pcm <- PCM(pcmdat2)
  plotPImap(res.pcm, sorted = TRUE)
  pres.pcm <- person.parameter(res.pcm)
  itemfit(pres.pcm)
  
  lr <- 2 * (res.pcm$loglik - res.rsm$loglik)
  df <- res.pcm$npar - res.rsm$npar
  pvalue <- 1 - pchisq(lr, df)
  cat("LR statistic: ", lr, " df =", df, " p =", pvalue, "\n")
  
  return(res.pcm)
}

exampleFour_function <- function(){
  aov_out <- aov(yield ~ N*P, npk)
  model.tables(aov_out, "means") # This will give you the means.
  model.tables(aov_out, se = TRUE) # Will give you standard errors for the effects.
  model.tables(aov_out, "means", se = TRUE) # Will give you the standard error for the differences of means.
  return(aov_out)
}

setwd("./cats")
document()

#source: https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
