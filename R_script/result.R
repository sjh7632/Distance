#install.packages("foreign")
library(foreign)

#install.packages("stringr")
library(stringr)

fl <- list.files(path="training_testing/testset/arff/out",recursive=T)

flist <- c()
d <- c()

for(i in 1:length(fl)) {
  ff <- read.table(paste0("training_testing/testset/arff/out/",fl[i]),header=F,sep="\n")
  
  flist <- c(flist,unlist(str_split(fl[i],"[.]"))[1])
  
  out <- tail(ff,n=7)
  Mae_t <- gsub(" ","",out[3,])
  Mae <- as.double(substr(Mae_t,18,str_length(Mae_t)))
  
  TnoI_t <- gsub(" ","",out[7,])
  TnoI <- as.integer(substr(TnoI_t,23,str_length(TnoI_t)))
  
  if(TnoI %% 2 == 0) {
    d <- c(d,Mae*(TnoI%/%2))
  } else {
    d <- c(d,Mae*((TnoI+1)%/%2))
  }
} 

fframe <- data.frame("File"=flist,"Distance"=d)

write.csv(fframe,"model_distance_smo.csv",row.names=F)