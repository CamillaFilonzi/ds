library(readr)
library(lessR)
library(qdap)
library(dplyr)
library(tm)
library(corpus)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(RWeka)
library(reshape2)
library(quanteda)
library(textstem)
library(tidyr)

#import dataset ######
disney=read.csv("/Users/CamillaFilonzi/Disney/disney.csv", stringsAsFactors = FALSE)
names(disney)


disney = sample_n(disney, 150)

#trasform column film year into double type 
#and replace NA with 0 (in order to delete it in stopwords)
disney$film_year = as.double(disney$film_year)
disney[is.na(disney)] <- 0

#divide in decades
disney$decade = NA

disney = disney  %>%
  mutate(decade = case_when(
    is.na(decade) & film_year > 1949 & film_year < 1960 ~ 1950,
    TRUE  ~ as.double(decade)
  ))

disney = disney  %>%
  mutate(decade = case_when(
    is.na(decade) & film_year > 1959 & film_year < 1970 ~ 1960,
    TRUE  ~ as.double(decade)
  ))

disney = disney  %>%
  mutate(decade = case_when(
    is.na(decade) & film_year > 1969 & film_year < 1980 ~ 1970,
    TRUE  ~ as.double(decade)
  ))


disney = disney  %>%
  mutate(decade = case_when(
    is.na(decade) & film_year > 1979 & film_year < 1990 ~ 1980,
    TRUE  ~ as.double(decade)
  ))


disney = disney  %>%
  mutate(decade = case_when(
    is.na(decade) & film_year > 1989 & film_year < 2000 ~ 1990,
    TRUE  ~ as.double(decade)
  ))

disney = disney  %>%
  mutate(decade = case_when(
    is.na(decade) & film_year > 1999 ~ 2000,
    TRUE  ~ as.double(decade)
  ))

#merging title and text reviews

disney$title_text <- paste(disney$title_name, disney$review_text)

#WORDCOUNT
disney$wordcount = ""
disney$wordcount = word_count(disney$title_text)

#creating a simple dataframe with just film name, reviews, year 
#disney1 = disney[, c("film_name", "ID", "title_review", "film_year")]



# library(tidyverse)
# 
# disney$try = ""
# 
# df = disney %>%
#   fill(try) %>%
#   group_by(decade) %>%
#   summarise(try = paste(title_review, collapse = " "))


# #globale 
# 
# disney$globale = NA
# disney$globale1 = ""
# 
# df2= disney %>%
#   fill(globale1) %>%
#   group_by(globale) %>%
#   summarise(globale1 = paste(title_review, collapse = " "))
############
disney1 = disney[,c("film_name","decade", "title_text", "wordcount")]

corpus_globale=Corpus(VectorSource(disney1$title_text))
corpus_globale=tm_map(corpus_globale, tolower)
corpus_globale=tm_map(corpus_globale, removePunctuation)
corpus_globale=tm_map(corpus_globale, removeWords, stopwords("en"))
corpus_globale= tm_map(corpus_globale, lemmatize_strings)
corpus_globale = tm_map(corpus_globale, removeWords,c("cinderella","alice","wonderland","peterpan","sword","stone","junglebook",
                                                      "marypoppins","aristocats","rescuers", "winne","pooh","littlemarmaid","ariel",
                                                      "tron","oliver","aladdin","lion","king","hercules","emperor","spirited","away","lilo","stitch",
                                                      "film","movie","0","disney","also","make" ,"get","like", "made", "can", "im", "i", "just", 
                                                      "watch", "see", "still", "go", "one"))

GLOBALE = as.data.frame(corpus_globale)

disney1$title_text =GLOBALE$text


library(Xplortext)
# Utilizziamo la variabile album per aggregare i documenti
res.corpus <- TextData(disney1,var.text=names(disney1)[3],var.agg = names(disney1)[2],
                       context.quali=c(names(disney1)[1]),
                       idiom="en",lower=FALSE, remov.number=FALSE,
                       graph = TRUE, stop.word.tm=FALSE)
plot(res.corpus)


# Analisi delle corrispondenze lessicali
res.LexCA<-LexCA(res.corpus, graph = FALSE)
summary(res.LexCA,metaWords=TRUE)
plot(res.LexCA,eigen=TRUE)


# Mappe semantiche

plot(res.LexCA,selDoc=NULL, col.word="red",cex=1,
     title="Terms representation")

plot(res.LexCA,col.word="red", col.doc="blue",cex=0.8,
     title="Documents and terms representation")


plot(res.LexCA,selWord=top50_words$word,cex=1,col.doc="blue",
     title="Documents and top 50 words")

ellipseLexCA(res.LexCA,selWord=NULL,col.doc="grey30",
             title="Confidence ellipses around the documents")









library(Xplortext)

res.TD1<-TextData(GLOBALE, var.text=2,remov.number=TRUE, Fmin=3, Dmin=3)
res.TD1$indexW

##tabella lessicale
TableLex<-as.matrix(res.TD1$DocTerm)
TableLex
##LCA
res.LexCA<-LexCA(res.TD1)
res.hc<-LexHCca(res.LexCA, nb.clust=3, graph=TRUE)

#classificazione doppia clustering gerarchico completo + k-means
res.hc<-LexHCca(res.LexCA,order=TRUE,nb.clust=3,
                graph=FALSE)
#studio dei cluster
res.hc$clust.count
#descrizione dei cluster
res.hc$description$desc.cluster.doc$words

#produco le stesse analisi utilizzando un altro pacchetto
library(FactoMineR)
library(factoextra)
res.CA<-CA(TableLex)
hc<-HCPC(res.CA)

fviz_screeplot(res.CA, addlabels = TRUE, ylim = c(0, 8))


# 


fviz_ca_row(res.CA, col.row = "contrib",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)

library(tm) 
library(ggplot2) 
library(lsa)

#inspect(corpus_globale)

#wordcloud
tdm<-TermDocumentMatrix(corpus_globale)
# transform tdm into a matrix
life <- as.matrix(tdm)
v <- sort(rowSums(life),decreasing=TRUE) 
d <- data.frame(word = names(v),freq=v)

#wordcloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word, col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word, col =heat.colors(10), main ="Most frequent words", ylab = "Word frequencies")

#bello questo
install.packages("wordcloud2")
library(wordcloud2)
wordcloud2(data = d)


comparison.cloud(TL, random.order=FALSE)






