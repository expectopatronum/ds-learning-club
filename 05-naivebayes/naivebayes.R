# https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data

mush_data<-read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data", sep=",", header=FALSE)

mush_test<-sample(1:nrow(mush_data), nrow(mush_data)*0.1)

#### naive bayes ####
library(e1071)
nb_mush <- naiveBayes(mush_data[-mush_test, paste0("V",2:23)], mush_data[-mush_test, "V1"])
pred_mush <- predict(nb_mush, mush_data[mush_test, paste0("V",2:23)])
sum(pred_mush==mush_data[mush_test, "V1"])

#### build AUC ####

pred_train <- predict(nb_mush, mush_data[-mush_test, paste0("V",2:23)])
#calcAUC
