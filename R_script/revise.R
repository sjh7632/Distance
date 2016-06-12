fl <- list.files(path="original",recursive=T)

for (i in 1:length(fl)) {
  fname <- paste0("original/",fl[i])
  ff <- read.csv(fname,header=T)
  
  result <- paste0("revision/",fl[i])
  
  accel_ts <- c(0)
  accel_x <- c(0)
  accel_y <- c(0)
  accel_z <- c(0)
  gyro_ts <- c(0)
  gyro_x <- c(0)
  gyro_y <- c(0)
  gyro_z <- c(0)
  
  ts_a <- c()
  ts_g <- c()
  for (i in 1:(length(ff$accel_ts)-1)) {
    ts_a[i] <- ff$accel_ts[i+1] - ff$accel_ts[i]
    ts_g[i] <- ff$gyro_ts[i+1] - ff$gyro_ts[i]
  }
  ts_am <- mean(ts_a)
  ts_gm <- mean(ts_g)
  
  ts_af <- ts_am
  ts_gf <- ts_gm
  
  for (i in 1:length(ts_a)) {
    ts_af[i+1] <- ts_a[i]
    ts_gf[i+1] <- ts_g[i]
  }
  
  ts_af <- ts_af / 1000000000
  ts_gf <- ts_gf / 1000000000
  
  for (i in 1:length(ts_af)) {
    accel_ts[i+1] <- accel_ts[i] + ts_af[i]
    gyro_ts[i+1] <- gyro_ts[i] + ts_gf[i]
  }
  
  for(i in 1:length(ff$accel_x)) {
    accel_x[i+1] <- ff$accel_x[i]
    accel_y[i+1] <- ff$accel_y[i]
    accel_z[i+1] <- ff$accel_z[i]
    gyro_x[i+1] <- ff$gyro_x[i]
    gyro_y[i+1] <- ff$gyro_y[i]
    gyro_z[i+1] <- ff$gyro_z[i]
  }
  
  data <- data.frame(accel_ts,accel_x,accel_y,accel_z,gyro_ts,gyro_x,gyro_y,gyro_z)
  
  write.csv(data,result,row.names=F)
}
  


