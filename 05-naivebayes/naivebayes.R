#### plots ####

ggplot(mush_data_melt[mush_data_melt$variable == "cap-shape",], aes(x = value, fill = edible)) + geom_bar() + theme(axis.text.x = element_text(angle = 90)) + scale_fill_brewer(palette="Spectral")
ggplot(mush_data_melt[mush_data_melt$variable == "cap-surface",], aes(x = value, fill = edible)) + geom_bar() + theme(axis.text.x = element_text(angle = 90)) + scale_fill_brewer(palette="Spectral")


ggplot(mush_data_melt[mush_data_melt$variable == "cap-shape",], aes(x = edible, fill = value)) + geom_bar() + theme(axis.text.x = element_text(angle = 90)) + scale_fill_brewer(palette="Spectral")

#### naive bayes ####
library(e1071)
training_features <- names(mush_data)[-1]
target <- "edible"

training_features <- "odor"

nb_mush <- naiveBayes(data.frame("odor"=mush_data[-mush_test, training_features]), mush_data[-mush_test, target])
pred_mush_test <- predict(nb_mush, data.frame("odor"=mush_data[mush_test, training_features]))
pred_mush_train <- predict(nb_mush, mush_data[-mush_test, training_features])
corr1_test <- sum(pred_mush_test==mush_data[mush_test, target])
corr1_train <- sum(pred_mush_train==mush_data[-mush_test, target])

nb_mush_thr <- nb_mush
nb_mush_thr$tables$x[nb_mush_thr$tables$x == 0] <- 0.001
nb_mush_thr$tables$x[1,] <- nb_mush_thr$tables$x[1,] / sum(nb_mush_thr$tables$x[1,])
nb_mush_thr$tables$x[2,] <- nb_mush_thr$tables$x[2,] / sum(nb_mush_thr$tables$x[2,])

any(predict(nb_mush_thr, mush_data[mush_test, training_features]) == "p")


table(pred_mush_test, mush_data[mush_test, target])
table(pred_mush_train, mush_data[-mush_test, target])

#### build AUC ####

pred_train <- predict(nb_mush, mush_data[-mush_test, paste0("V",2:23)])
#calcAUC

library(tableplot)
tableplot(as.matrix(mush_data), )
