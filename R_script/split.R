#install.packages("stringr")
#library(stringr)

fl <- list.files(path="revision",recursive=T)

fend <- 0

for (i in 1:length(fl)) {
  fname <- paste0("revision/",fl[i])
  ff <- read.csv(fname,header=T)
  
  result0 <- unlist(str_split(fl[i],"[.]"))[1]
  
  first <- 0
  end <- 0.5
  fcount <- 1
  while (TRUE) {
    rr <- subset(ff,accel_ts >= first & accel_ts < end)
    result <- paste0("split/",result0,"_",as.character(fcount),".csv")
    
    if(fend == rr$accel_ts[length(rr$accel_ts)]) {
      break
    }
    
    write.csv(rr,result,row.names=F)
    
    first <- median(rr$accel_ts)
    end <- first + 0.5
    fcount <- fcount + 1  
    
    fend <- rr$accel_ts[length(rr$accel_ts)]
  }
}