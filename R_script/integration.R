#setwd("E:/Project/Wearable/data/moving")

#install.packages("stringr")
#library(stringr)

fl <- list.files(path="revision",recursive=T)

v <- c(as.double(0))
d <- c(as.double(0))
fd <- c()
flist <- c()

for (i in 1:length(fl)) {
  ff <- read.csv(paste0("revision/",fl[i]),header=T)
  
  for (j in 2:nrow(ff)) {
    tmp <- v[j-1] + as.double(ff[,2][j-1]) + ((as.double(ff[,2][j])-as.double(ff[,2][j-1]))/2) *
      (as.double(ff[,1][j])-as.double(ff[,1][j-1]))
    v <- c(v,tmp)
  }
  for (j in 2:nrow(ff)) {
    tmpd <- d[j-1] + v[j-1] +((v[j]-v[j-1])/2)*(as.double(ff[,1][j])-as.double(ff[,1][j-1]))
    d <- c(d,tmpd)
  }
  fd <- c(fd,sum(abs(tmpd)))
  
  fname <- unlist(str_split(fl[i],"/"))[2]
  tn <- unlist(str_split(substr(fname,1,str_length(fname)-4),"_"))
  flist <- c(flist,paste0(tn[1],"_",tn[4],"_",tn[5],"_",tn[6]))  
}
fframe <- data.frame("File"=flist,"Distance"=fd)

write.csv(fframe,"integrate_distance.csv",row.names=F)

