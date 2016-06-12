#install.packages("stringr")
library(stringr)

attribute.remove <- "java weka.filters.unsupervised.attribute.Remove "
test <- "java weka.classifiers.functions.SMOreg "


original <- "result_moving-u.arff"
training <- "result_moving_s.arff"


fl <- list.files(path="testset/arff",recursive=F)

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

  order <- c(order,paste0(test,"-c 2 -x 10 -s 5 -t ",training," -T ",fl[i]," > ",fout," &"))
}

write(order,"testset/arff/testing_model_SMOreg.bat",sep="\n")