# Load libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(readr)
library(data.table)
library(mlr)
library(checkmate)


# Nexessary functions to check valid label names
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


# Feature engineering
# MLR makeMultilabelTask requires proper R naming convention
# What are the invalid labels?
colnames(datamin)[!isValidAndUnreservedName(colnames(datamin))]

# Let's replace dash to underscore
colnames(datamin) <- gsub("\\-", "_", colnames(datamin))

# Test again
colnames(datamin)[!isValidAndUnreservedName(colnames(datamin))]

# Clean more
colnames(datamin) <- gsub("\\#", "sharp", colnames(datamin))
colnames(datamin) <- gsub("\\+", "plus", colnames(datamin))
colnames(datamin) <- gsub("64", "sixtyfour", colnames(datamin))
colnames(datamin) <- gsub("function", "f_unction", colnames(datamin))
colnames(datamin) <- gsub("3d", "threed", colnames(datamin))


# Convert datetimes to numeric valuse (requeried to makeMultilabelTask)
datamin$CreationDate <- as.numeric(as.POSIXct(datamin$CreationDate))
datamin$ClosedDate <- as.numeric(as.POSIXct(datamin$ClosedDate))
datamin$DeletionDate <- as.numeric(as.POSIXct(datamin$DeletionDate))


# Convert zeros and ones to logical true and falses
datamin[8:ncol(datamin)] <- lapply(datamin[8:ncol(datamin)], as.logical)



#datamin$CreationDate <- NULL
#datamin$ClosedDate <- NULL
#datamin$DeletionDate <- NULL




# Craetea Multilabel classifier task
task = makeMultilabelTask(id = "multi", data = datamin[1:1000], target = colnames(datamin[8:ncol(datamin)]))
task


# Create binary learner
lrn.br = makeLearner("classif.rpart", predict.type = "prob") 
lrn.br = makeMultilabelBinaryRelevanceWrapper(lrn.br) 
lrn.br 


mod = train(lrn.br, task) 
mod



pred = predict(mod, task = task) 
pred = predict(mod, newdata = datamin[1001:1100,]) 
names(as.data.frame(pred))



resp <- getPredictionResponse(pred)
df <- as.data.frame(resp)
df$python_2.6==T

df_truth <- as.data.frame(getPredictionTruth(pred))
df$arrays



getPredictionSE(pred)

glimpse(getPredictionProbabilities(pred))
probs <- getPredictionProbabilities(pred)

glimpse(as.data.frame(getPredictionResponse(pred)))

performance(pred)
listMeasures("multilabel")



getMultilabelBinaryPerformances(pred, measures = list(acc, mmce, auc))
performance(pred)


