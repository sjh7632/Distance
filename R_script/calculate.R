### 패키지
#install.packages("entropy")
library(entropy)

#install.packages("e1071")
library(e1071)

#install.packages("foreign")
library(foreign)

#install.packages("stringr")
library(stringr)

### 파일 리스트
fl <- list.files(path="split",recursive=T)

flist <- c()

for(i in 1:length(fl)) {
  aa <- unlist(str_split(fl[i],"/"))[2]
  flist <- c(flist,unlist(str_split(aa,"[.]"))[1])
}

### 유저 리스트
ulist <- c()

for(i in 1:length(fl)) {
  uu <- unlist(str_split(fl[i],"/"))[1]
  ulist <- c(ulist,uu)
}

### 프레임 리스트
frlist <- c()

for(i in 1:length(fl)) {
  fr <- unlist(str_split(fl[i],"arm_"))[2]
  frlist <- c(frlist,unlist(str_split(fr,"[.]"))[1])
}

### 전체 시간, 거리
ft <- c()
fd <- c()

revision <- read.csv("revision_full.csv",header=T)

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

### 프레임 시간, 거리
frt <- c()
frd <- c()

### feature vector 초기화
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

### feature vector 생성
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

### 데이터 프레임 생성
result <- data.frame("Filename"=flist,"User"=ulist,"Frame"=frlist,
                     "Full_Time"=ft,"Full_Distance"=fd,
                     "Frame_Time"=frt,"Frame_Distance"=frd,
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

### 파일 쓰기
write.csv(result,"result_moving.csv",row.names=T)
write.arff(result,"result_moving.arff")