
## Libraries
library(tidyverse)
library(caret)

## Read in the Data
fakeNews <- read_csv("CleanFakeNews.csv")

fakeNews$isFake <- as.factor(fakeNews$isFake)

fakeNewsTemp.train <- fakeNews %>% filter(!is.na(isFake))
fakeNewsTemp.test <- fakeNews %>% filter(is.na(isFake))

rm(fakeNews)

fakeNewsTemp.train$isFake <- as.factor(fakeNewsTemp.train$isFake)
fakeNewsTemp.test$isFake <- as.factor(fakeNewsTemp.test$isFake)

fakeNewsTemp.test$author_type <- ifelse(fakeNewsTemp.test$author_type == "unknown", "unreliable", fakeNewsTemp.test$author_type)

fakeNewsTemp.train <- fakeNewsTemp.train %>% select(-lan)
fakeNewsTemp.test <- fakeNewsTemp.test %>% select(-lan) 

#fakeNewsTemp.train <- fakeNewsTemp.train[sample(nrow(fakeNewsTemp.train), 50), 1:12]

tune.grid = expand.grid(n.trees = c(50, 100, 200, 500),
interaction.depth = seq(3, 10, 1),
shrinkage = c(.1, .2, .01),
n.minobsinnode = seq(5, 15, 1))

boost <- train(form=isFake~., 
               data=(fakeNewsTemp.train %>% select(-Id)),
               method = "gbm",
               trControl=trainControl(method="repeatedcv",
                                      number= 5,
                                      repeats = 3),
               tuneGrid = tune.grid,
               verbose = FALSE
)
beepr::beep()

plot(boost)
boost$bestTune
boost$results
print("Run Complete")

imdb.preds <- data.frame(Id=fakeNewsTemp.test$Id, label = predict(boost, newdata = fakeNewsTemp.test))

save(list=c("boost", "imdb.preds"), file="./Preds.RData")
write_csv(x=imdb.preds, path="./Peters_Submission.csv")
