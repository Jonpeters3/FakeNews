
## Libraries
library(tidyverse)
library(caret)

## Read in the Data
fakeNews <- read_csv("CleanFakeNews.csv")

fakeNewsTemp <- fakeNews[,1:12]

rm(fakeNews)

fakeNewsTemp.train <- fakeNewsTemp %>% filter(!is.na(isFake))
fakeNewsTemp.test <- fakeNewsTemp %>% filter(is.na(isFake))


tune.grid = expand.grid(n.trees = c(50, 100, 150),
                        interaction.depth = c(2, 3, 4),
                        shrinkage = .1,
                        n.minobsinnode = 10)

boost <- train(form=isFake~., 
               data=(fakeNewsTemp.train %>% select(-Id)),
               method = "gbm",
               trControl=trainControl(method="repeatedcv",
                                      number=10,
                                      repeats = 5),
               preProc = c("center","scale"),
               tuneGrid = tune.grid,
               verbose = FALSE
)

plot(boost)
boost$bestTune
boost$results
```


imdb.preds <- data.frame(Id=fakeNewsTemp.test$Id, Predicted = predict(boost, newdata = fakeNewsTemp.test))

write_csv(x=imdb.preds, path="./Peters_Submission_Boost.csv")