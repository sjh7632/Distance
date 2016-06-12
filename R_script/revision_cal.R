fl <- list.files(path="revision",recursive=T)

flist <- c()

for(i in 1:length(fl)) {
  f <- unlist(str_split(fl[i],"/"))[2]
  flist <- c(flist,unlist(str_split(f,"[.]"))[1])
}

full_t <- c()

full_dist <- c()
  
for(i in 1:length(fl)) {
  ff <- read.csv(paste0("revision/",fl[i]),header=T)
  
  t <- ff$accel_ts[nrow(ff)]-ff$accel_ts[1]
  
  full_t <- c(full_t,t)
  
  d <- unlist(str_split(unlist(str_split(fl[i],"arm_"))[2],"_"))[1]
  
  full_dist <- c(full_dist,d)
}


result <- data.frame("Filename"=flist,"Full_Time"=full_t,"Full_Distance"=full_dist)

write.csv(result,"revision_full.csv",row.names=T)