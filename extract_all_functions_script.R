#go to R folder
R_path <- setwd("/Users/wendywang/Dropbox/eRm-master/R")
eRm_fn_dirs <- list.files(path = ".", pattern = NULL, all.files = FALSE, full.names = FALSE, recursive = FALSE)
eRm_fn_total <- length(eRm_fn_dirs)
counter <- 1
#=============================== Create Astrom library file ===================================
libPath<-file("/Users/wendywang/Dropbox/RPackage/RPackageTest/AstromERM.R")

#=============================== Astrom library starts here ====================================
while (counter < eRm_fn_total){
  print(counter)
  fnPath <- file(eRm_fn_dirs[counter], open="r")
  line <- readLines(fnPath, n=1)
  print(line)
  write(line, file=libPath, append=TRUE, sep = "\n")
  close(fnPath)
  counter <- counter + 1
}

#================================ Finish writing Astrom file ======================================
close(filePath)