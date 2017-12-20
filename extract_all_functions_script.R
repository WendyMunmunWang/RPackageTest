#go to R folder
R_path <- setwd("/Users/wendywang/Dropbox/eRm-master/R")
eRm_fn_dirs <- list.dirs(path = getwd(), full.names = TRUE, recursive = TRUE)
eRm_fn_total <- length(eRm_fn_dirs)