# https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data

mush_data<-read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data", sep=",", header=FALSE)
mush_test<-sample(1:nrow(mush_data), nrow(mush_data)*0.1)

names(mush_data) <- c("edible",
                      "cap-shape",
                      "cap-surface",
                      "cap-color",
                      "bruises",
                      "odor",
                      "gill-attachment",
                      "gill-spacing",
                      "gill-size",
                      "gill-color",
                      "stalk-shape",
                      "stalk-root",
                      "stalk-surface-above-ring",
                      "stalk-surface-below-ring",
                      "stalk-color-above-ring",
                      "stalk-color-below-ring",
                      "veil-type",
                      "veil-color",
                      "ring-number",
                      "ring-type",
                      "spore-print-color",
                      "population",
                      "habitat")


mush_data_melt <- melt(mush_data, id.vars = "edible")

save(mush_data, mush_data_melt,  mush_test, file="05-naivebayes/data.RData")
