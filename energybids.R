#setting the working directory.
setwd("D:/analytics edge/energy bids recetation")
#importing the energy_bids.csv file from system in emails variable.
emails=read.csv("energy_bids.csv", stringsAsFactors = F)
#visualising the structure of the emails. 
str(emails)
emails$email[1]
#using strwrap() function to see the whole data in emails as str() function only shows few lines of the data.
strwrap(emails$email[1])
emails$responsive[1]
strwrap(emails$email[2])
emails$responsive[2]
#using table fucntion to see how may zeros and ones are there in responsive.
table(emails$responsive)
#importing package tm.
library(tm)
# now managing the document in the main structure using corpus(). where vectorsource() function interprets each element of vector as a document.
corpus=Corpus(VectorSource(emails$email))
strwrap(corpus[[1]])
# now converting the strings in corpus to lower character.
corpus<- tm_map(corpus, tolower)
#removing punctuation in corpus.
corpus<- tm_map(corpus, removePunctuation)
# now we'r removing the stopwords using removeWords and we'll stop by the english words.
corpus<- tm_map(corpus,removeWords, stopwords("english"))
corpus<- tm_map(corpus, stemDocument)
strwrap(corpus[[1]])
#now converting the corpus to document term matrix.
dtm= DocumentTermMatrix(corpus)
dtm
# now to remove the words which are not appear in atleast 3% of the time.
dtm= removeSparseTerms(dtm, 0.97)
dtm
#converting dtm to data frame.
labeledTerms<- as.data.frame(as.matrix(dtm))
labeledTerms$responsive<- emails$responsive
str(labeledTerms)
labeledTerms$responsive
library(caTools)
set.seed(144)
#splitting the data in two parts.
spl=sample.split(labeledTerms$responsive, 0.7)
#training set
train<- subset(labeledTerms, spl==T)
#test set
test<- subset(labeledTerms, spl==F)
library(rpart)
library(rpart.plot)
#using rpart to plot the model.
emailCART<- rpart(responsive~., data=train, method="class")
prp(emailCART)
#making predictions using predict function.
pred<- predict(emailCART, newdata = test)
pred[1:10,]
pred.prob<- pred[,2]
table(test$responsive, pred.prob>=0.5)
library(ROCR)
predROCR<- prediction(pred.prob, test$responsive)
predROCR
perfROCR<- performance(predROCR, "tpr","fpr")
plot(perfROCR,colorize=T)
performance(predROCR, "auc")@y.values
