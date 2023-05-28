library(tm)
library(wordcloud)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

reviews<-read.csv(file.choose(),header=T)

str(reviews)

corpus<-iconv(reviews$text)
corpus<-Corpus(VectorSource(corpus))
inspect(corpus[1:5])

corpus<-tm_map(corpus,tolower)
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,removeNumbers)
corpus<-tm_map(corpus,removeWords,stopwords("english"))
corpus<-tm_map(corpus,stripWhitespace)
corpus<-tm_map(corpus,removeWords,c("book","page","chapters","paper","read","life","wabi","sabi","japan","japanese"))
inspect(corpus[1:5])

review_final<-corpus

tdm<-TermDocumentMatrix(review_final)
tdm<-as.matrix(tdm)
tdm[1:10,1:5]

w<-rowSums(tdm)
w<-subset(w,w>=10)
barplot(w,las=2,col="lightblue")

w<-sort(rowSums(tdm),decreasing = T)
set.seed(2000)
wordcloud(words=names(w),
          freq=w,
          max.words=50,
          random.order = T,
          min.freq=5,
          colors = brewer.pal(25,"Dark2"),
          scale = c(3,0.3))

sen_dt<-iconv(reviews$text)
s_dt<-get_nrc_sentiment(sen_dt)
s_dt[1:10,]

s_dt$score<-s_dt$positive-s_dt$negative
s_dt[1:10,]

write.csv(x=s_dt,file="/Users/K/Desktop/Kaizen.csv")

review_score<-colSums(s_dt[,])
print(review_score)

barplot(colSums(s_dt),
        las=2,
        col=rainbow(10),
        ylab="count",
        main="sentiment")
