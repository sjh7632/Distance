#install.packages("stringr")
library(stringr)

#install.packages("foreign")
library(foreign)

ff <- read.csv("result_moving.csv",header=T)

df <- unlist(str_split(ff[1,4],"_"))
uu <- as.character(ff[1,3])

ff$Frame_Distance <- NULL

fm <- cbind(Frame_Time=ff[,7],class=0)
fm <- cbind(fm,ff[8:length(ff)])

fout <- data.frame()

for (i in 1:nrow(ff)) {
  tp <- unlist(str_split(ff[i,4],"_"))
  ut <- as.character(ff[i,3])

  if((uu!=ut)||(df[1]!=tp[1])||(df[2]!=tp[2])||(df[3]!=tp[3])) {
    uu <- ut; df[1] <- tp[1]; df[2] <- tp[2]; df[3] <- tp[3]
    
    write.csv(fout,fname.csv,row.names=T)
    write.arff(fout,fname.arff)
    
    fout <- data.frame()
  }
  
  if(tp[4] == "1") {
    fname.csv <- paste0("testset/csv/",ut,"_",tp[1],"_",tp[2],"_",tp[3],".csv")
    fname.arff <- paste0("testset/arff/",ut,"_",tp[1],"_",tp[2],"_",tp[3],".arff")
  }
  
  fout <- rbind(fout,fm[i,])
}

write.csv(fout,fname.csv,row.names=T)
write.arff(fout,fname.arff)