library(readr)
library(dplyr)


#import dataset ######
disney=read.csv("C:/Users/Mario/Desktop/Tor Vergata Data Science/Data Warehousing/disney_exam/disney_new.csv", stringsAsFactors = FALSE)

#samples di 60 (ma in realta 100)
disney <- disney %>% group_by(film_name) %>% sample_n(100)

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
    is.na(decade) & film_year > 1999 & film_year < 2010 ~ 2000,
    TRUE  ~ as.double(decade)
  ))

#dopo il nuovo db
disney = disney  %>%
  mutate(decade = case_when(
    is.na(decade) & film_year > 2009 ~ 2010,
    TRUE  ~ as.double(decade)
  ))
#merging title and text reviews

disney$text = paste(disney$title_name, disney$review_text)


n_distinct(disney$decade)


#WORDCOUNT
#disney$wordcount = ""
#disney$wordcount = word_count(disney$title_text)

#dataframe definitivo

disney1 <- cbind(doc_id = 1:nrow(disney), disney)

disney1 = as.data.frame(disney1)

#creo dataframe tidytext
disney1 = disney1[,c("doc_id","text","film_name","decade")]  #"wordcount"

#disney1 = as.data.frame(disney1)

library(tm)
df_sorgente = DataframeSource(disney1)
disney_corpus = VCorpus(df_sorgente)
meta(disney_corpus)
# Or even
#meta(disney_corpus_1[[545]])


disney_corpus_1=tm_map(disney_corpus, content_transformer(tolower))
disney_corpus_1=tm_map(disney_corpus_1, content_transformer(removePunctuation))
disney_corpus_1=tm_map(disney_corpus_1, content_transformer(stripWhitespace))
disney_corpus_1=tm_map(disney_corpus_1, content_transformer(removeWords), stopwords("en"))

disney_corpus_1 = tm_map(disney_corpus_1, content_transformer(removeWords),c("na","cinderella","alice","wonderland","peterpan","sword","stone","junglebook",
                                                                             "marypoppins","aristocats","rescuers", "winne","pooh","littlemarmaid","ariel",
                                                                             "tron","oliver","aladdin","lion","king","hercules","emperor","spirited","away","lilo","stitch",
                                                                             "film","movie","0","disney","also","make" ,"get","like", "made", "can", "im", "i", "just", 
                                                                             "watch", "see", "still", "go", "one"))


disney_corpus_1= tm_map(disney_corpus_1, content_transformer(lemmatize_strings))

library(textstem)

library(tidytext)
library(tidyverse)

dataframe<-data.frame(text=unlist(sapply(disney_corpus_1, `[`, "content")), 
                      stringsAsFactors=F)
dataframe$decade = disney1$decade
dataframe$film = disney1$film_name

mask = (meta(disney_corpus_1)$decade == 1950) + 0
corpus_1950 = disney_corpus_1[mask]

mask = (meta(disney_corpus_1)$decade == 1960) + 0
corpus_1960 = disney_corpus_1[mask]

mask = (meta(disney_corpus_1)$decade == 1970) + 0
corpus_1970 = disney_corpus_1[mask]

mask = (meta(disney_corpus_1)$decade == 1980) + 0
corpus_1980 = disney_corpus_1[mask]

mask = (meta(disney_corpus_1)$decade == 1990) + 0
corpus_1990 = disney_corpus_1[mask]

mask = (meta(disney_corpus_1)$decade == 2000) + 0
corpus_2000 = disney_corpus_1[mask]

mask = (meta(disney_corpus_1)$decade == 2010) + 0
corpus_2010 = disney_corpus_1[mask]



#   {
#     paste(content(disney_corpus_1[mask])[i])
#   }


# prende parole piu frequenti e le conta per film
c = dataframe %>%
  unnest_tokens(word, text) %>%
  count(film, word, sort = TRUE)
#per decade
c1 = dataframe %>%
  unnest_tokens(word, text) %>%
  count(decade, word, sort = TRUE)

# find the words most distinctive to each document
c %>%
  bind_tf_idf(word, film, n) %>%
  arrange(desc(tf_idf))


#analisi corrispondenze lessicali ##############
#bag of words
tdm_bag = TermDocumentMatrix(disney_corpus_1)
#inspect(tdm_bag)



# transform tdm into a matrix
tdm_m <- as.matrix(tdm_bag)
values <- sort(rowSums(tdm_m),decreasing=TRUE)
terms <- data.frame(word = names(values),freq=values)
rownames(terms)<-NULL
terms$rank<-c(1:length(terms$word))
##estraiamo per frequenza
top50_words<-terms[1:50,]
top50_words

top100_words = terms[1:100,]



#Document term
dtm_disney_corpus <- DocumentTermMatrix(disney_corpus_1)


library(ggplot2) 
library(lsa)
library(wordcloud)

#wordcloud
set.seed(1234)
wordcloud(words = terms$word, freq = terms$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word, col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word, col =heat.colors(10), main ="Most frequent words", ylab = "Word frequencies")

#bello questo

library(wordcloud2)
wordcloud2(data = terms)


----
  
  
#analisi concordanze (del contesto)
#decidi se farla 
library(koRpus)
library(quanteda)


t<-tokens(as.character(disney_corpus_1))
multiword <- c("good", "bad") 
head(kwic(t, pattern = phrase(multiword)))



#---------------
 library(Xplortext)
disney2 = dataframe 
# Utilizziamo la variabile album per aggregare i documenti
res.corpus <- TextData(disney2,var.text=names(disney2)[3],var.agg =names(disney2)[1],
                       context.quali=c(names(disney2)[2]),
                       idiom="en",lower=TRUE, remov.number=FALSE,
                       graph = TRUE, stop.word.tm=TRUE)
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


ellipseLexCA(res.LexCA,selWord="contrib 10",col.doc="grey30",
             title="Confidence ellipses around the documents")
?ellipseLexCA




#Clusterizzare le parole delle recensioni, applicando algoritmi gerarchici e non gerarchici sia sulle
#coordinate fattoriali sia sulla matrice Tfidf. 
#Descrivere i principali risultati.
#I risultati dovranno essere presentati sia in forma globale che per decade



library(Xplortext)

res.TD1<-TextData(disney2, var.text=2,remov.number=TRUE, Fmin=3, Dmin=3)
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













