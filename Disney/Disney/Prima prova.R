
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
    is.na(ID) & film_name == "Hercules" ~ "HERC",
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
#STEP1 --Subsets and Corpus ######
#creating subset for each film 

#Decade '50
#Cenerentola
# using subset function
Cenerentola <- subset(disney1, film_name=="Cenerentola",
                      select=film_name:film_year)
#Corpus 
corpus_Cenerentola=Corpus(VectorSource(Cenerentola$title_review))

#Alice nel paese delle meraviglie
Alice <- subset(disney1, film_name=="Alice nel paese delle meraviglie",
                select=film_name:film_year)
#Corpus
corpus_Alice=Corpus(VectorSource(Alice$title_review))

#Peter pan
Peter_pan <- subset(disney1, film_name=="Peter pan",
                    select=film_name:film_year)
#Corpus
corpus_PeterPan=Corpus(VectorSource(Peter_pan$title_review))



# anni '60
#La Spada nella Roccia
Spada_roccia <- subset(disney1, film_name=="La Spada nella Roccia",
                       select=film_name:film_year)
#Corpus
corpus_SpadaRoccia =Corpus(VectorSource(Spada_roccia$title_review))

#The Jungle Book
Jungle_Book<- subset(disney1, film_name=="The Jungle Book",
                     select=film_name:film_year)
#Corpus
corpus_JungleBook=Corpus(VectorSource(Jungle_Book$title_review))

#Mary Poppins
Mary_Poppins <- subset(disney1, film_name=="Mary Poppins",
                       select=film_name:film_year)
#Corpus
corpus_MaryPoppins=Corpus(VectorSource(Mary_Poppins$title_review))

#anni '70
#The Aristocats
Aristocats = subset(disney1, film_name=="The Aristocats",
                    select=film_name:film_year)
#Corpus
corpus_Aristocats=Corpus(VectorSource(Aristocats$title_review))



#The Rescuers
Rescuers = subset(disney1, film_name=="The Rescuers",
                  select=film_name:film_year)
#Corpus
corpus_Rescuers=Corpus(VectorSource(Rescuers$title_review))



#The Many Adventures of Winnie the Pooh
Winnie_pooh = subset(disney1, film_name=="The Many Adventures of Winnie the Pooh",
                     select=film_name:film_year)
#Corpus
corpus_WinniePooh=Corpus(VectorSource(Winnie_pooh$title_review))



#anni '80
#The Little Mermaid
Little_mermaid = subset(disney1, film_name=="The Little Mermaid",
                        select=film_name:film_year)
#Corpus
corpus_LittleMermaid=Corpus(VectorSource(Little_mermaid$title_review))

#Tron
Tron = subset(disney1, film_name=="Tron",
              select=film_name:film_year)
#Corpus
corpus_Tron=Corpus(VectorSource(Tron$title_review))


#Oliver & Company
Oliver_eCo = subset(disney1, film_name=="Oliver & Company",
                    select=film_name:film_year)
#Corpus
corpus_Oliver=Corpus(VectorSource(Oliver_eCo$title_review))



#anni '90
#Aladdin
Aladdin = subset(disney1, film_name=="Aladdin",
                 select=film_name:film_year)
#Corpus
corpus_Aladdin=Corpus(VectorSource(Aladdin$title_review))


#The Lion King
Lion_king = subset(disney1, film_name=="The Lion King",
                   select=film_name:film_year)
#Corpus
corpus_LionKing=Corpus(VectorSource(Lion_king$title_review))


#Hercules
Hercules = subset(disney1, film_name=="Hercules",
                  select=film_name:film_year)
#Corpus
corpus_Hercules=Corpus(VectorSource(Hercules$title_review))


#anni '00
#The Emperor's New Groove
Emperor = subset(disney1, film_name=="The Emperor's New Groove",
                 select=film_name:film_year)
#Corpus
corpus_Emperor=Corpus(VectorSource(Emperor$title_review))


#Spirited Away
Spirited_Away = subset(disney1, film_name=="Spirited Away",
                       select=film_name:film_year)
#Corpus
corpus_SpiritedAway=Corpus(VectorSource(Spirited_Away$title_review))

#Lilo & Stitch
Lilo_Stitch = subset(disney1, film_name=="Lilo & Stitch",
                     select=film_name:film_year)
#Corpus
corpus_LiloStitch=Corpus(VectorSource(Lilo_Stitch$title_review))



# STEP2 — Text Pre-processing ########

#To Lower 
corpus_Cenerentola = tm_map(corpus_Cenerentola, tolower)
corpus_Alice = tm_map(corpus_Alice, tolower)
corpus_PeterPan = tm_map(corpus_PeterPan, tolower)
corpus_SpadaRoccia = tm_map(corpus_SpadaRoccia, tolower)
corpus_JungleBook = tm_map(corpus_JungleBook, tolower)
corpus_MaryPoppins = tm_map(corpus_MaryPoppins, tolower)
corpus_Aristocats = tm_map(corpus_Aristocats, tolower)
corpus_Rescuers = tm_map(corpus_Rescuers, tolower)
corpus_WinniePooh = tm_map(corpus_WinniePooh, tolower)
corpus_LittleMermaid = tm_map(corpus_LittleMermaid, tolower)
corpus_Tron = tm_map(corpus_Tron, tolower)
corpus_Oliver= tm_map(corpus_Oliver, tolower)
corpus_Aladdin= tm_map(corpus_Aladdin, tolower)
corpus_LionKing= tm_map(corpus_LionKing, tolower)
corpus_Hercules= tm_map(corpus_Hercules, tolower)
corpus_Emperor= tm_map(corpus_Emperor, tolower)
corpus_SpiritedAway= tm_map(corpus_SpiritedAway, tolower)
corpus_LiloStitch= tm_map(corpus_LiloStitch, tolower)

#Remove Punctuation

corpus_Cenerentola = tm_map(corpus_Cenerentola, removePunctuation)
corpus_Alice = tm_map(corpus_Alice, removePunctuation)
corpus_PeterPan = tm_map(corpus_PeterPan, removePunctuation)
corpus_SpadaRoccia = tm_map(corpus_SpadaRoccia, removePunctuation)
corpus_JungleBook = tm_map(corpus_JungleBook, removePunctuation)
corpus_MaryPoppins = tm_map(corpus_MaryPoppins, removePunctuation)
corpus_Aristocats = tm_map(corpus_Aristocats, removePunctuation)
corpus_Rescuers = tm_map(corpus_Rescuers, removePunctuation)
corpus_WinniePooh = tm_map(corpus_WinniePooh, removePunctuation)
corpus_LittleMermaid = tm_map(corpus_LittleMermaid, removePunctuation)
corpus_Tron = tm_map(corpus_Tron, removePunctuation)
corpus_Oliver= tm_map(corpus_Oliver, removePunctuation)
corpus_Aladdin= tm_map(corpus_Aladdin, removePunctuation)
corpus_LionKing= tm_map(corpus_LionKing, removePunctuation)
corpus_Hercules= tm_map(corpus_Hercules, removePunctuation)
corpus_Emperor= tm_map(corpus_Emperor, removePunctuation)
corpus_SpiritedAway= tm_map(corpus_SpiritedAway, removePunctuation)
corpus_LiloStitch= tm_map(corpus_LiloStitch, removePunctuation)

#remove estra white spaces
corpus_Cenerentola = tm_map(corpus_Cenerentola, stripWhitespace)
corpus_Alice = tm_map(corpus_Alice, stripWhitespace)
corpus_PeterPan = tm_map(corpus_PeterPan, stripWhitespace)
corpus_SpadaRoccia = tm_map(corpus_SpadaRoccia, stripWhitespace)
corpus_JungleBook = tm_map(corpus_JungleBook, stripWhitespace)
corpus_MaryPoppins = tm_map(corpus_MaryPoppins, stripWhitespace)
corpus_Aristocats = tm_map(corpus_Aristocats, stripWhitespace)
corpus_Rescuers = tm_map(corpus_Rescuers, stripWhitespace)
corpus_WinniePooh = tm_map(corpus_WinniePooh, stripWhitespace)
corpus_LittleMermaid = tm_map(corpus_LittleMermaid, stripWhitespace)
corpus_Tron = tm_map(corpus_Tron, stripWhitespace)
corpus_Oliver= tm_map(corpus_Oliver, stripWhitespace)
corpus_Aladdin= tm_map(corpus_Aladdin, stripWhitespace)
corpus_LionKing= tm_map(corpus_LionKing, stripWhitespace)
corpus_Hercules= tm_map(corpus_Hercules, stripWhitespace)
corpus_Emperor= tm_map(corpus_Emperor, stripWhitespace)
corpus_SpiritedAway= tm_map(corpus_SpiritedAway, stripWhitespace)
corpus_LiloStitch= tm_map(corpus_LiloStitch, stripWhitespace)


## Lemm document

corpus_Cenerentola = tm_map(corpus_Cenerentola, lemmatize_strings)
corpus_Alice = tm_map(corpus_Alice, lemmatize_strings)
corpus_PeterPan = tm_map(corpus_PeterPan, lemmatize_strings)
corpus_SpadaRoccia = tm_map(corpus_SpadaRoccia, lemmatize_strings)
corpus_JungleBook = tm_map(corpus_JungleBook, lemmatize_strings)
corpus_MaryPoppins = tm_map(corpus_MaryPoppins, lemmatize_strings)
corpus_Aristocats = tm_map(corpus_Aristocats, lemmatize_strings)
corpus_Rescuers = tm_map(corpus_Rescuers, lemmatize_strings)
corpus_WinniePooh = tm_map(corpus_WinniePooh, lemmatize_strings)
corpus_LittleMermaid = tm_map(corpus_LittleMermaid, lemmatize_strings)
corpus_Tron = tm_map(corpus_Tron, lemmatize_strings)
corpus_Oliver= tm_map(corpus_Oliver, lemmatize_strings)
corpus_Aladdin= tm_map(corpus_Aladdin, lemmatize_strings)
corpus_LionKing= tm_map(corpus_LionKing, lemmatize_strings)
corpus_Hercules= tm_map(corpus_Hercules, lemmatize_strings)
corpus_Emperor= tm_map(corpus_Emperor, lemmatize_strings)
corpus_SpiritedAway= tm_map(corpus_SpiritedAway, lemmatize_strings)
corpus_LiloStitch= tm_map(corpus_LiloStitch, lemmatize_strings)


#Remove stopwords

corpus_Cenerentola = tm_map(corpus_Cenerentola, removeWords, stopwords("english"))
corpus_Alice = tm_map(corpus_Alice, removeWords, stopwords("english"))
corpus_PeterPan = tm_map(corpus_PeterPan, removeWords, stopwords("english"))
corpus_SpadaRoccia = tm_map(corpus_SpadaRoccia, removeWords, stopwords("english"))
corpus_JungleBook = tm_map(corpus_JungleBook, removeWords, stopwords("english"))
corpus_MaryPoppins = tm_map(corpus_MaryPoppins, removeWords, stopwords("english"))
corpus_Aristocats = tm_map(corpus_Aristocats, removeWords, stopwords("english"))
corpus_Rescuers = tm_map(corpus_Rescuers, removeWords, stopwords("english"))
corpus_WinniePooh = tm_map(corpus_WinniePooh, removeWords, stopwords("english"))
corpus_LittleMermaid = tm_map(corpus_LittleMermaid, removeWords, stopwords("english"))
corpus_Tron = tm_map(corpus_Tron, removeWords, stopwords("english"))
corpus_Oliver= tm_map(corpus_Oliver, removeWords, stopwords("english"))
corpus_Aladdin= tm_map(corpus_Aladdin, removeWords, stopwords("english"))
corpus_LionKing= tm_map(corpus_LionKing, removeWords, stopwords("english"))
corpus_Hercules= tm_map(corpus_Hercules, removeWords, stopwords("english"))
corpus_Emperor= tm_map(corpus_Emperor, removeWords, stopwords("english"))
corpus_SpiritedAway= tm_map(corpus_SpiritedAway, removeWords, stopwords("english"))
corpus_LiloStitch= tm_map(corpus_LiloStitch, removeWords, stopwords("english"))






# Remove context specific stop words --- decide!!!!!!!!!!!!!!!! 
#--> movie?film? main character names? see? make? one? 

corpus_Cenerentola = tm_map(corpus_Cenerentola, removeWords,c("cinderella","film","movie","0","disney","also","make" ,"get","like", "made", "can", "im", "i", "just", "watch", "see", "still", "go", "one"))
corpus_Alice = tm_map(corpus_Alice, removeWords,c("alice","wonderland","film","movie","0","disney","also", "make", "get","like", "made", "can", "im", "i", "just", "watch", "see", "still", "go", "one"))
corpus_PeterPan = tm_map(corpus_PeterPan, removeWords,c("peterpan","film","movie","0","disney","also", "make", "get","like", "made", "can", "im", "i", "just", "watch", "see", "still", "go", "one"))
corpus_SpadaRoccia = tm_map(corpus_SpadaRoccia, removeWords,c("sward","stone","film","movie","0","disney","also", "make", "get","like", "made", "can", "im", "i", "just", "watch", "see", "still", "go", "one"))
corpus_JungleBook = tm_map(corpus_JungleBook, removeWords,c("junglebook","film","movie","0","disney","also", "make", "get","like", "made", "can", "im", "i", "just", "watch", "see", "still", "go", "one"))
corpus_MaryPoppins = tm_map(corpus_MaryPoppins, removeWords,c("marypoppins","film","movie","0","disney","also", "make", "get","like", "made", "can", "im", "i", "just", "watch", "see", "still", "go", "one"))
corpus_Aristocats = tm_map(corpus_Aristocats, removeWords,c("aristocats","film","movie","0","disney","also", "make", "get","like", "made", "can", "im", "i", "just", "watch", "see", "still", "go", "one"))
corpus_Rescuers = tm_map(corpus_Rescuers, removeWords,c("rescuers","film","movie","0","disney","also", "make", "get","like", "made", "can", "im", "i", "just", "watch", "see", "still", "go", "one"))
corpus_WinniePooh = tm_map(corpus_WinniePooh, removeWords,c("winne","pooh","film","movie","0","disney","also", "make", "get","like", "made", "can", "im", "i", "just", "watch", "see", "still", "go", "one"))
corpus_LittleMermaid = tm_map(corpus_LittleMermaid, removeWords,c("littlemarmaid","ariel","film","movie","0","disney","also", "make", "get","like", "made", "can", "im", "i", "just", "watch", "see", "still", "go", "one"))
corpus_Tron = tm_map(corpus_Tron, removeWords,c("tron","film","movie","0","disney","also", "make", "get","like", "made", "can", "im", "i", "just", "watch", "see", "still", "go", "one"))
corpus_Oliver= tm_map(corpus_Oliver, removeWords,c("oliver","film","movie","0","disney","also", "make", "get","like", "made", "can", "im", "i", "just", "watch", "see", "still", "go", "one"))
corpus_Aladdin= tm_map(corpus_Aladdin, removeWords,c("aladdin","film","movie","0","disney","also", "make", "get","like", "made", "can", "im", "i", "just", "watch", "see", "still", "go", "one"))
corpus_LionKing= tm_map(corpus_LionKing, removeWords,c("lion","king","film","movie","0","disney","also", "make", "get","like", "made", "can", "im", "i", "just", "watch", "see", "still", "go", "one"))
corpus_Hercules= tm_map(corpus_Hercules, removeWords,c("hercules","film","movie","0","disney","also", "make", "get","like", "made", "can", "im", "i", "just", "watch", "see", "still", "go", "one"))
corpus_Emperor= tm_map(corpus_Emperor, removeWords,c("emperor","film","movie","0","disney","also", "make", "get","like", "made", "can", "im", "i", "just", "watch", "see", "still", "go", "one"))
corpus_SpiritedAway= tm_map(corpus_SpiritedAway, removeWords,c("spirited","away","film","movie","0","disney","also", "make", "get","like", "made", "can", "im", "i", "just", "watch", "see", "still", "go", "one"))
corpus_LiloStitch= tm_map(corpus_LiloStitch, removeWords,c("lilo","stitch","film","movie","0","disney","also", "make", "get","like", "made", "can", "im", "i", "just", "watch", "see", "still", "go", "one"))

#corpus_LiloStitch= tm_map(corpus_LiloStitch, removeWords,c("film", "make"))


##Viewing the corpus content
#corpus_SpiritedAway$content



# Find the 20 most frequent terms: term_count

freq_terms_Cenerentola = freq_terms(corpus_Cenerentola,40)
freq_terms_Alice = freq_terms(corpus_Alice,30)
freq_terms_PeterPan = freq_terms(corpus_PeterPan,30)
freq_terms_SpadaRoccia = freq_terms(corpus_SpadaRoccia,30)
freq_terms_JungleBook = freq_terms(corpus_JungleBook,30)
freq_terms_MaryPoppins = freq_terms(corpus_MaryPoppins,30)
freq_terms_Aristocats = freq_terms(corpus_Aristocats,30)
freq_terms_Rescuers = freq_terms(corpus_Rescuers,30)
freq_terms_WinniePooh = freq_terms(corpus_WinniePooh,30)
freq_terms_LittleMermaid = freq_terms(corpus_LittleMermaid,30)
freq_terms_Tron = freq_terms(corpus_Tron,30)
freq_terms_Oliver= freq_terms(corpus_Oliver,30)
freq_terms_Aladdin= freq_terms(corpus_Aladdin,30)
freq_terms_LionKing= freq_terms(corpus_LionKing,30)
freq_terms_Hercules= freq_terms(corpus_Hercules,30)
freq_terms_Emperor= freq_terms(corpus_Emperor,30)
freq_terms_SpiritedAway= freq_terms(corpus_SpiritedAway,30)
freq_terms_LiloStitch= freq_terms(corpus_LiloStitch,30)


# Plot 20 most frequent terms
#par(mfrow=c(6,3))
plot(freq_terms_Cenerentola) 
plot(freq_terms_Alice )
plot(freq_terms_PeterPan) 
plot(freq_terms_SpadaRoccia )
plot(freq_terms_JungleBook )
plot(freq_terms_MaryPoppins)
plot(freq_terms_Aristocats)
plot(freq_terms_Rescuers)
plot(freq_terms_WinniePooh)
plot(freq_terms_LittleMermaid)
plot(freq_terms_Tron)
plot(freq_terms_Oliver)
plot(freq_terms_Aladdin)
plot(freq_terms_LionKing)
plot(freq_terms_Hercules)
plot(freq_terms_Emperor)
plot(freq_terms_SpiritedAway)
plot(freq_terms_LiloStitch)

#creo Corpus Decadi 

decade_50 = c(corpus_PeterPan, corpus_Alice, corpus_Cenerentola)


corpus_Cenerentola_1 <- TermDocumentMatrix(corpus_Cenerentola)
corpus_Cenerentola_matr=as.matrix(corpus_Cenerentola_1)


typeof(decade_50)

commonality.cloud(corpus_Cenerentola_matr, 
                  colors = "steelblue1",
                  max.words = 50)
