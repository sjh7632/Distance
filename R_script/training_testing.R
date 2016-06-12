#install.packages("foreign")
library(foreign)

#install.packages("stringr")
library(stringr)

fl <- list.files(path="revision",recursive=T)

ftrain <- c()
ftest<- c()

for (i in 1:length(fl)) {
	if((i %% 3) == 0) {
		ftest <- c(ftest,fl[i])
		next
	}
	ftrain <- c(ftrain,fl[i])	
}

#######################################################################

#install.packages("stringr")
library(stringr)

fend <- 0

for (i in 1:length(ftrain)) {
  fname <- paste0("revision/",ftrain[i])
  ff <- read.csv(fname,header=T)
  
  result0 <- unlist(str_split(ftrain[i],"[.]"))[1]
  
  first <- 0
  end <- 0.5
  fcount <- 1
  while (TRUE) {
    rr <- subset(ff,accel_ts >= first & accel_ts < end)
    result <- paste0("training_testing/split/",result0,"_",as.character(fcount),".csv")
    
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

#########################################################################

flist <- c()

for(i in 1:length(ftrain)) {
  f <- unlist(str_split(ftrain[i],"/"))[2]
  flist <- c(flist,unlist(str_split(f,"[.]"))[1])
}

full_t <- c()

full_dist <- c()

for(i in 1:length(ftrain)) {
  ff <- read.csv(paste0("revision/",ftrain[i]),header=T)
  
  t <- ff$accel_ts[nrow(ff)]-ff$accel_ts[1]
  
  full_t <- c(full_t,t)
  
  d <- unlist(str_split(unlist(str_split(ftrain[i],"arm_"))[2],"_"))[1]
  
  full_dist <- c(full_dist,d)
}


result <- data.frame("Filename"=flist,"Full_Time"=full_t,"Full_Distance"=full_dist)

write.csv(result,"training_testing/revision_full_t.csv",row.names=T)


########################################################################

### ???????ـ
#install.packages("entropy")
library(entropy)

#install.packages("e1071")
library(e1071)

#install.packages("foreign")
library(foreign)

#install.packages("stringr")
library(stringr)

### ?????? 리스???
fl <- list.files(path="training_testing/split",recursive=T)

flist <- c()

for(i in 1:length(fl)) {
  aa <- unlist(str_split(fl[i],"/"))[2]
  flist <- c(flist,unlist(str_split(aa,"[.]"))[1])
}

### ?????? 리스???
ulist <- c()

for(i in 1:length(fl)) {
  uu <- unlist(str_split(fl[i],"/"))[1]
  ulist <- c(ulist,uu)
}

### ????????? 리스???
frlist <- c()

for(i in 1:length(fl)) {
  fr <- unlist(str_split(fl[i],"arm_"))[2]
  frlist <- c(frlist,unlist(str_split(fr,"[.]"))[1])
}

### ?????? ??????, 거리
ft <- c()
fd <- c()

revision <- read.csv("training_testing/revision_full_t.csv",header=T)

for(i in 1:length(fl)) {
  for(j in 1:length(revision$Filename)) {
    tmp <- substr(flist[i],1,str_length(revision$Filename[j]))
    if (tmp == revision$Filename[j]) {
      ft <- c(ft,revision$Full_Time[j])
      fd <- c(fd,revision$Full_Distance[j])
      break
    }
  }
}

### ????????? ??????, 거리
frt <- c()
frd <- c()

### feature vector 초기???
acc_mean_x <- c(); acc_mean_y <- c(); acc_mean_z <- c()
acc_std_x <- c(); acc_std_y <- c(); acc_std_z <- c()
acc_mad_x <- c(); acc_mad_y <- c(); acc_z_mad <- c()
acc_max_x <- c(); acc_max_y <- c(); acc_max_z <- c()
acc_min_x <- c(); acc_min_y <- c(); acc_min_z <- c()
acc_sma <- c()
acc_energy_x <- c(); acc_energy_y <- c(); acc_energy_z <- c()
acc_iqr_x <- c(); acc_iqr_y <- c(); acc_iqr_z <- c()
acc_entropy_x <- c(); acc_entropy_y <- c(); acc_entropy_z <- c()
acc_correlation_xy <- c(); acc_correlation_xz <- c(); acc_correlation_yz <- c()
acc_skewness_x <- c(); acc_skewness_y <- c(); acc_skewness_z <- c()
acc_kurtosis_x <- c(); acc_kurtosis_y <- c(); acc_kurtosis_z <- c()

### feature vector ??????
for (i in 1:length(fl)) {
  fname <- paste0("split/",fl[i])
  ff <- read.csv(fname,header=T)
  
  frt <- c(frt,(ff$accel_ts[length(ff$accel_ts)]-ff$accel_ts[1]))
  frd <- c(frd,(fd[i] * frt[i])/ft[i])
  
  acc_sma <- c(acc_sma,mean(abs(ff$accel_x)+abs(ff$accel_y)+abs(ff$accel_z)))
  acc_correlation_xy <- c(acc_correlation_xy,cor(ff$accel_x,ff$accel_y))
  acc_correlation_xz <- c(acc_correlation_xz,cor(ff$accel_x,ff$accel_z))
  acc_correlation_yz <- c(acc_correlation_yz,cor(ff$accel_y,ff$accel_z))
  
  acc_mean_x <- c(acc_mean_x,mean(ff$accel_x))
  acc_std_x <- c(acc_std_x,sd(ff$accel_x))
  acc_mad_x <- c(acc_mad_x,median(ff$accel_x))
  acc_max_x <- c(acc_max_x,max(ff$accel_x))
  acc_min_x <- c(acc_min_x,min(ff$accel_x))
  acc_energy_x <- c(acc_energy_x,mean((ff$accel_x)^2))
  acc_iqr_x <- c(acc_iqr_x,IQR(ff$accel_x))
  acc_entropy_x <- c(acc_entropy_x,entropy(ff$accel_x))
  acc_skewness_x <- c(acc_skewness_x,skewness(ff$accel_x))
  acc_kurtosis_x <- c(acc_kurtosis_x,kurtosis(ff$accel_x))
  
  acc_mean_y <- c(acc_mean_y,mean(ff$accel_y))
  acc_std_y <- c(acc_std_y,sd(ff$accel_y))
  acc_mad_y <- c(acc_mad_y,median(ff$accel_y))
  acc_max_y <- c(acc_max_y,max(ff$accel_y))
  acc_min_y <- c(acc_min_y,min(ff$accel_y))
  acc_energy_y <- c(acc_energy_y,mean((ff$accel_y)^2))
  acc_iqr_y <- c(acc_iqr_y,IQR(ff$accel_y))
  acc_entropy_y <- c(acc_entropy_y,entropy(ff$accel_y))
  acc_skewness_y <- c(acc_skewness_y,skewness(ff$accel_y))
  acc_kurtosis_y <- c(acc_kurtosis_y,kurtosis(ff$accel_y))
  
  acc_mean_z <- c(acc_mean_z,mean(ff$accel_z))
  acc_std_z <- c(acc_std_z,sd(ff$accel_z))
  acc_z_mad <- c(acc_z_mad,median(ff$accel_z))
  acc_max_z <- c(acc_max_z,max(ff$accel_z))
  acc_min_z <- c(acc_min_z,min(ff$accel_z))
  acc_energy_z <- c(acc_energy_z,mean((ff$accel_z)^2))
  acc_iqr_z <- c(acc_iqr_z,IQR(ff$accel_z))
  acc_entropy_z <- c(acc_entropy_z,entropy(ff$accel_z))
  acc_skewness_z <- c(acc_skewness_z,skewness(ff$accel_z))
  acc_kurtosis_z <- c(acc_kurtosis_z,kurtosis(ff$accel_z))
}

### ????????? ????????? ??????
result <- data.frame("Filename"=flist,"User"=ulist,"Frame"=frlist,
                     "Full_Time"=ft,"Full_Distance"=fd,
                     "Frame_Time"=frt,"class"=frd,
                     "Acc_mean_X"=acc_mean_x,"Acc_mean_Y"=acc_mean_y,"Acc_mean_Z"=acc_mean_z,
                     "Acc_std_X"=acc_std_x,"Acc_std_Y"=acc_std_y,"Acc_std_Z"=acc_std_z,
                     "Acc_mad_X"=acc_mad_x,"Acc_mad_Y"=acc_mad_y,"Acc_mad_Z"=acc_z_mad,
                     "Acc_max_X"=acc_max_x,"Acc_max_Y"=acc_max_y,"Acc_max_Z"=acc_max_z,
                     "Acc_min_X"=acc_min_x,"Acc_min_Y"=acc_min_y,"Acc_min_Z"=acc_min_z,
                     "Acc_sma"=acc_sma,
                     "Acc_energy_X"=acc_energy_x,"Acc_energy_Y"=acc_energy_y,"Acc_energy_Z"=acc_energy_z,
                     "Acc_iqr_X"=acc_iqr_x,"Acc_iqr_Y"=acc_iqr_y,"Acc_iqr_Z"=acc_iqr_z,
                     "Acc_entropy_X"=acc_entropy_x,"Acc_entropy_Y"=acc_entropy_y,"Acc_entropy_Z"=acc_entropy_z,
                     "Acc_correlation_X,Y"=acc_correlation_xy,"Acc_correlation_X,Z"=acc_correlation_xz,"Acc_correlation_Y,Z"=acc_correlation_yz,
                     "Acc_skewness_X"=acc_skewness_x,"Acc_skewness_Y"=acc_skewness_y,"Acc_skewness_Z"=acc_skewness_z,
                     "Acc_kurtosis_X"=acc_kurtosis_x,"Acc_kurtosis_Y"=acc_kurtosis_y,"Acc_kurtosis_Z"=acc_kurtosis_z
)

### ?????? ??????
write.csv(result,"training_testing/result_moving_t.csv",row.names=T)
write.arff(result,"training_testing/result_moving_t.arff")

####################################################################################

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
  if( unlist(str_split(ff[i,4],"_"))[3] != "3") next
  
  tp <- unlist(str_split(ff[i,4],"_"))
  ut <- as.character(ff[i,3])
  
  if(tp[4] == "1") {
    fname.csv <- paste0("training_testing/testset/csv/",ut,"_",tp[1],"_",tp[2],"_",tp[3],".csv")
    fname.arff <- paste0("training_testing/testset/arff/",ut,"_",tp[1],"_",tp[2],"_",tp[3],".arff")
  }
  
  if((uu!=ut)||(df[1]!=tp[1])||(df[2]!=tp[2])||(df[3]!=tp[3])) {
    uu <- ut; df[1] <- tp[1]; df[2] <- tp[2]; df[3] <- tp[3]
    
    write.csv(fout,fname.csv,row.names=T)
    write.arff(fout,fname.arff)
    
    fout <- data.frame()
  }
  
  fout <- rbind(fout,fm[i,])
}

write.csv(fout,fname.csv,row.names=T)
write.arff(fout,fname.arff)

####################################################################

#install.packages("stringr")
library(stringr)

attribute.remove <- "java weka.filters.unsupervised.attribute.Remove "
test <- "java weka.classifiers.meta.Bagging "


original <- "result_moving_t.arff"
training <- "result_moving_w.arff"


fl <- list.files(path="training_testing/testset/arff",recursive=F)

for (i in 1: length(fl)) {
  if(substr(fl[i],str_length(fl[i])-3,str_length(fl[i])) != "arff") {
    fl[i] <- NA
  } else if(substr(fl[i],1,6) == "result") {
    fl[i] <- NA
  }
}
fl <- fl[!is.na(fl)]

order <- c("mkdir out")
order <- c(order,paste0(attribute.remove,"-R 1-5 -c 2 -i ",original," -o ",training," &"))

for (i in 1: length(fl)) {
  fout <- paste0("out\\",substr(fl[i],1,str_length(fl[i])-5),".out")
  
  order <- c(order,paste0(test,"-c 2 -x 10 -s 10 -t ",training," -T ",fl[i]," > ",fout," &"))
}

write(order,"training_testing/testset/arff/testing_model_t.bat",sep="\n")