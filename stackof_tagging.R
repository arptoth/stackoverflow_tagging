# Load libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(readr)
library(data.table)
library(mlr)
library(checkmate)

# Load questions and tags by fread
questions <- fread("~/Downloads/questions.csv")
tags <- fread("~/Downloads/question_tags.csv")

# Due to limitations choose 1000 row randomly
datamin <- sample_n(questions, 1000, replace = FALSE)

# Use only those tags which are related to this 1000 questions
tags <- subset(tags, Id %in% datamin$Id)

#Let's look at the data
glimpse(datamin)
glimpse(tags)


# What are the most popular tags?
tags %>%
  group_by(Tag) %>% 
  summarise(Count = n()) %>% # Summarize number of tags
  arrange(desc(Count))


# Join tags to data by id
datamin <- left_join(datamin, tags, by = "Id")
glimpse(datamin)

# Create a one-hot features from labels
for(unique_value in unique(datamin$Tag)){
  datamin[unique_value] <- ifelse(datamin$Tag == unique_value, 1, 0)
}

# Remove Tag column
datamin$Tag <- NULL

# Store labels
labels = colnames(datamin[8:ncol(datamin)])
labels





colnames(datamin) <- gsub("\\.|\\+|\\#", "", colnames(datamin))
colnames(datamin) <- gsub("\\-", "_", colnames(datamin))
colnames(datamin) <- gsub("[[:punct:]]", "", colnames(datamin))


datamin$DeletionDate <- NULL
datamin$DeletionDate <- NULL
datamin$DeletionDate <- NULL

duplicated(labels)


names(datamin) 


labels

gsub("[[:punct:]]", "", labels)


task = makeMultilabelTask(id = "multi", data = datamin, target = colnames(datamin[8:ncol(datamin)]))
task



lrn.br = makeLearner("classif.rpart", predict.type = "prob")
lrn.br = makeMultilabelBinaryRelevanceWrapper(lrn.br)
lrn.br

mod = train(lrn.br, task)
mod

pred = predict(mod, task = task, subset = 1:10)
pred = predict(mod, newdata = datamin[1501:1600,])
names(as.data.frame(pred))


getPredictionResponse(pred)


getPredictionSE(pred)
getPredictionTruth(pred)
getPredictionProbabilities(pred)
performance(pred)
listMeasures("multilabel")


colnames(datamin[5:ncol(datamin)]) <- make.names(labels)


ifelse(datamin$require==1, T, F)

for (i in names(datamin)) {
  ifelse(datamin$i==1, T, F)
}

datamin[5:ncol(datamin)] <- lapply(datamin[5:ncol(datamin)], as.logical)


for (year in c(2010,2011,2012,2013,2014,2015)){
  print(paste("The year is", year))
}



colnames(datamin)[!isValidAndUnreservedName(colnames(datamin))]

isValidName <- function(string) {
  grepl("^([[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*$", string)
}

isValidAndUnreservedName <- function(string) {
  make.names(string) == string
}

testValidity <- function(string) {
  valid <- isValidName(string)
  unreserved <- isValidAndUnreservedName(string)
  reserved <- (valid & ! unreserved)
  list("Valid"=valid,
       "Unreserved"=unreserved,
       "Reserved"=reserved)
}
