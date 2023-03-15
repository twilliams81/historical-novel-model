#I borrowed some of this code from Andrew Piper and amended it for my own purposes.


#to train on one data set and run on another
#load training data and relabel corpus
setwd("set WD")
histNovelDF<-read.csv("historical_novel_list.csv", stringsAsFactors = T)
notHistNovelDF<-read.csv("non_historical_novel_list.csv", stringsAsFactors = T)
histNovelDF$corpus <- c("HIST")
notHistNovelDF$corpus <- c("NOTHIST")


#check to see if columns match
which(colnames(histNovelDF) != colnames(notHistNovelDF))

#combine into single data frame
dfHist <- rbind(histNovelDF, notHistNovelDF)
dfHist<-dfHist[,-1]#removes title column
dfHist<-dfHist[,-1] #removes word count column

dfHist$corpus <- as.factor(dfHist$corpus) #makes sure there are no character vectors in the DF


#Let's see how accurate the predictions can be

folds<-createFolds(dfHist$corpus, k=2) # k = number of folds I'm using 2 because so little data
#

cv.results<-lapply(folds, function(x){
  dfHist.train<-dfHist[-x,]
  dfHist.test<-dfHist[x,]
  dfHist.model<-ksvm(corpus ~ ., data=dfHist.train, kernel="rbfdot")
  dfHist.pred<-predict(dfHist.model, dfHist.test)
  con.matrix<-confusionMatrix(dfHist.pred, dfHist.test$corpus, positive = "HIST") #change this identifier for each corpus
  f1<-con.matrix$byClass[[7]] 
  #p<-con.matrix$overall[[6]]
})
unlist(cv.results) #this shows you the F1 score for each fold
mean(unlist(cv.results)) #this is the average F1 score for all folds


#Now let's test it on a larger data set of contemporary fiction

#load test data and relabel corpus
#
#
contNovels <- read.csv("LIWC_CONT_NOV_3P.csv", stringsAsFactors = T)
#clean

contNovelsClean <- contNovels[,-1] #removes titles
contNovelsClean <- contNovelsClean [,-1] #removes corpus col
contNovelsClean <- contNovelsClean [,-1] #removes WC as feature
contNovelsClean$corpus <- ("unknown")

#check to see if columns match
which(colnames(dfHist) != colnames(contNovelsClean))


contNovelsClean$corpus <- as.factor(contNovelsClean$corpus)# Turns corpus strings into Factor

#train model
hist.model<-ksvm(corpus ~ ., data=dfHist, kernel="rbfdot")

#test model
histPred<-predict(hist.model, contNovelsClean)


#binds the original file names to the the predictions
prediction_names <- bind_cols(histPred, contNovels$filenames)

write.csv2(prediction_names, "prediction.csv")

View(prediction_names)
