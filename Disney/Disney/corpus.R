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
library(Xplortext)

#import dataset ######
disney=read.csv("/Users/CamillaFilonzi/Disney/disney.csv", stringsAsFactors = FALSE)
names(disney)


disney$ID = NA

disney = disney  %>%
  mutate(ID = case_when(
    is.na(ID) & film_name == "Cenerentola" ~ "CIN",
    TRUE  ~ as.character(ID)
  ))

disney = disney  %>%
  mutate(ID = case_when(
    is.na(ID) & film_name == "Aladdin" ~ "AL",
    TRUE  ~ as.character(ID)
  ))

disney = disney  %>%
  mutate(ID = case_when(
    is.na(ID) & film_name == "The Rescuers" ~ "RES",
    TRUE  ~ as.character(ID)
  ))

disney = disney  %>%
  mutate(ID = case_when(
    is.na(ID) & film_name == "La Spada nella Roccia" ~ "ROC",
    TRUE  ~ as.character(ID)
  ))

disney = disney  %>%
  mutate(ID = case_when(
    is.na(ID) & film_name == "Mary Poppins" ~ "MP",
    TRUE  ~ as.character(ID)
  ))

disney = disney  %>%
  mutate(ID = case_when(
    is.na(ID) & film_name == "Peter pan" ~ "PP",
    TRUE  ~ as.character(ID)
  ))

disney = disney  %>%
  mutate(ID = case_when(
    is.na(ID) & film_name == "The Emperor's New Groove" ~ "EMP",
    TRUE  ~ as.character(ID)
  ))

disney = disney  %>%
  mutate(ID = case_when(
    is.na(ID) & film_name == "The Lion King" ~ "LK",
    TRUE  ~ as.character(ID)
  ))

disney = disney  %>%
  mutate(ID = case_when(
    is.na(ID) & film_name == "The Many Adventures of Winnie the Pooh" ~ "WP",
    TRUE  ~ as.character(ID)
  ))

disney = disney  %>%
  mutate(ID = case_when(
    is.na(ID) & film_name == "Tron" ~ "TR",
    TRUE  ~ as.character(ID)
  ))

disney = disney  %>%
  mutate(ID = case_when(
    is.na(ID) & film_name == "Alice nel paese delle meraviglie" ~ "APM",
    TRUE  ~ as.character(ID)
  ))

disney = disney  %>%
  mutate(ID = case_when(
    is.na(ID) & film_name == "Hercules" ~ "H",
    TRUE  ~ as.character(ID)
  ))

disney = disney  %>%
  mutate(ID = case_when(
    is.na(ID) & film_name == "Lilo & Stitch" ~ "LS",
    TRUE  ~ as.character(ID)
  ))

disney = disney  %>%
  mutate(ID = case_when(
    is.na(ID) & film_name == "Oliver & Company" ~ "OC",
    TRUE  ~ as.character(ID)
  ))

disney = disney  %>%
  mutate(ID = case_when(
    is.na(ID) & film_name == "Spirited Away" ~ "SA",
    TRUE  ~ as.character(ID)
  ))

disney = disney  %>%
  mutate(ID = case_when(
    is.na(ID) & film_name == "The Aristocats" ~ "AR",
    TRUE  ~ as.character(ID)
  ))

disney = disney  %>%
  mutate(ID = case_when(
    is.na(ID) & film_name == "The Jungle Book" ~ "JB",
    TRUE  ~ as.character(ID)
  ))

disney = disney  %>%
  mutate(ID = case_when(
    is.na(ID) & film_name == "The Little Mermaid" ~ "LM",
    TRUE  ~ as.character(ID)
  ))

#table(disney$ID, disney$film_year)

disney[is.na(disney)] <- 0

#merging title and text reviews
disney$title_review <- paste(disney$title_name, disney$review_text, disney$ID)

#creating a simple dataframe with just film name, reviews, year 
disney1 = disney[, c("film_name", "ID", "title_review", "film_year")]

#Corpus 
corpus_review=Corpus(VectorSource(disney1$title_review))

#tolower
corpus_review = tm_map(corpus_review, tolower)

#Remove Punctuation

corpus_review = tm_map(corpus_review, removePunctuation)

#remove estra white spaces
corpus_review = tm_map(corpus_review, stripWhitespace)


## Lemm document

corpus_review = tm_map(corpus_review, lemmatize_strings)

#Remove stopwords

corpus_review = tm_map(corpus_review, removeWords, stopwords("english"))

# Remove context specific stop words --- decide!!!!!!!!!!!!!!!! 
#--> movie?film? main character names? see? make? one? 

corpus_review = tm_map(corpus_review, removeWords,c("cinderella","film","movie","0","disney","also","make" ,"get","like", "made", "can", "im", "i", "just", "watch", "see", "still", "go", "one"))

# Find the 20 most frequent terms: term_count

freq_terms_Cenerentola = freq_terms(corpus_review,40)

# Plot 20 most frequent terms
#par(mfrow=c(6,3))
plot(freq_terms_Cenerentola) 


#wordcloud
corpusreview_1<- TermDocumentMatrix(corpus_review)
corpusreview_1_matr=as.matrix(corpusreview_1)

v <- sort(rowSums(corpusreview_1_matr),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#costruzione delle mappe semantiche
res.TD<-TextData(disney1, var.text=3, remov.number=TRUE, Fmin=3, Dmin=3,  stop.word.tm=TRUE)
res.TD$summGen
#distribuzione di frequenza
res.TD$indexW
##creazione di una lista di parole da eliminare
U_stoplist=c("cinderella","aladdin","rescuers","sward","stone","mary","poppins","peter","pan","peterpan","emperor","groove","lion","king","winnie","pooh","tron","alice","wonderland","hercules","lilo","stitch","oliver","spirited","away","aristocats","jungle","book","litte mermaid","film","movie","0","disney","also","make" ,"get","like", "made", "can", "im", "i", "just", "watch", "see", "still", "go", "one")
res.TD1<-TextData(disney1, var.text=3, remov.number=TRUE, Fmin=3, Dmin=3,  
                  stop.word.user=U_stoplist, stop.word.tm=TRUE)
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





