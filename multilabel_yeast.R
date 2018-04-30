library(mlr)
library(dyplr)

yeast = getTaskData(yeast.task)
labels = colnames(yeast)[1:14]


glimpse(yeast)

str(yeast)
yeast.task = makeMultilabelTask(id = "multi", data = yeast, target = labels)
yeast.task
