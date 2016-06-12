library(stringr)

i_dist <- read.csv("integrate_distance.csv",header=T)
m_dist <- read.csv("model_distance_smo.csv",header=T)

i_dist[unlist(str_split(i_dist[,1],"_"))[4]=="3",]

ni_dist <- data.frame()
origin <- c()

for(i in 1:nrow(i_dist)) {
  if(unlist(str_split(i_dist[i,1],"_"))[4]=="3") {
    ni_dist <- rbind(ni_dist,i_dist[i,])
    origin <- c(origin,unlist(str_split(i_dist[i,1],"_"))[2])
  }
}

I_ER <- ((as.double(ni_dist$Distance)-as.double(origin))/as.double(origin))*100
M_ER <- ((as.double(m_dist$Distance)-as.double(origin))/as.double(origin))*100

fframe <- data.frame("File"=m_dist$File,"Origin"=origin,
                     "Integrated"=ni_dist$Distance,"Modeling"=m_dist$Distance,
                     "Conventional"=I_ER,"Proposal"=M_ER)

write.csv(fframe,"report/result_compare_smo.csv",row.names=F)

