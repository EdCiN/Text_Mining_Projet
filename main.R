############################### Import #########################################
library(NLP)
library("tm")
library(RColorBrewer)
library(wordcloud)
library(readr)
data <- read_csv("abcnews-date-text.csv")
############################### Pre process ###################################
#dtype
sapply(data, class)

#Format date
data$publish_date <- as.character(data$publish_date)
data$publish_date <- as.Date(data$publish_date, format="%Y%m%d")


############################## Partie I ##############################
# Définir les dates de début et de fin
date_debut <- as.Date("2003-02-01")
date_fin <- as.Date("2003-05-01")
#data filtrer par la période
data_fev_juillet=data[data$publish_date >= date_debut & data$publish_date <= date_fin, ]

#corpus
crp=VCorpus(VectorSource(data_fev_juillet$headline_text))

#Cleaning
clean_crp <- tm_map(crp, content_transformer(tolower))
stopwords(kind="en")
clean_crp <- tm_map(clean_crp, removeWords, stopwords("english"))
clean_crp <- tm_map(clean_crp, removePunctuation)
clean_crp <- tm_map(clean_crp, removeNumbers)


#matrice
matrice=DocumentTermMatrix(clean_crp)
matrice=as.matrix(matrice) # lourd genre 1gigas
# Calculer la fréquence des mots
frequency <- colSums(matrice)

# Trier les mots par fréquence
frequency <- sort(frequency, decreasing = TRUE)
#Nuage de mots
words <- names(frequency)
wordcloud(words[1:100], word_freq[1:100])

######################Partie 2 ################################################

