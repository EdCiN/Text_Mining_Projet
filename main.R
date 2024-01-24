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


####################### Partie II ################################################

############## Création d'un dataframe avec les sujets Iraq, Covid, Climate et Other ##############################
library(dplyr)

library(stringr)

#Df
df <- data %>%
  mutate(topic = case_when(
    str_detect(headline_text, "\\biraq\\b") ~ "Iraq", 
    str_detect(headline_text, "\\bcovid\\b") ~ "Covid",
    str_detect(headline_text, "\\bclimate\\b") ~ "Climate",
    TRUE ~ "Other"
  ))

# On supprime les lignes qui contiennent le sujet "Other"
df <- df %>%
  filter(topic != "Other")

########################## On affiche les sujets et leur wordcloud ##############################
library(tm)
library(wordcloud)


# On divise le dataframe par sujet
data_split <- split(df$headline_text, df$topic)

#fonction pour nettoyer et calculer les fréquences de mots
get_word_frequencies <- function(texts) {
  corpus <- Corpus(VectorSource(texts))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("french"))
  dtm <- DocumentTermMatrix(corpus)
  frequency <- colSums(as.matrix(dtm))
  frequency <- sort(frequency, decreasing = TRUE)
  return(frequency)
}

# Calcule des fréquences de mots pour chaque sujet
frequencies <- lapply(data_split, get_word_frequencies)

#wordcloud pour chaque sujet
for (topic in names(frequencies)) {
  frequency <- frequencies[[topic]]
  words <- names(frequency)
  wordcloud(words[1:100], frequency[1:100], main = paste("Wordcloud for topic:", topic))
}



###################### Modèle Bayesien ########################################

### On regarde la proportion
class_counts <- table(df$topic)
class_proportions <- class_counts / nrow(df)
print(class_proportions)


###### Train/Test
# Charger le package
library(tibble)
library(caret)

set.seed(25) 
trainIndex <- createDataPartition(df$topic, p = .8, 
                                  list = FALSE, 
                                  times = 1, 
                        
                                  )

trainSet <- df[trainIndex, ]
testSet  <- df[-trainIndex, ]

##verif proportion dans le train et le test
#train
train_class_counts <- table(trainSet$topic)
train_class_proportions <- train_class_counts / nrow(trainSet)
print(train_class_proportions)

#test
test_class_counts <- table(testSet$topic)
test_class_proportions <- test_class_counts / nrow(testSet)
print(test_class_proportions)

#Les proportions sont similaires dans le train et le test

### Model bayesien

library(e1071)

#Corpus
crp= Corpus(VectorSource(trainSet$headline_text))
#Cleaning
clean_crp <- tm_map(crp, content_transformer(tolower))
stopwords(kind="en")
clean_crp <- tm_map(clean_crp, removeWords, stopwords("english"))
clean_crp <- tm_map(clean_crp, removePunctuation)
clean_crp <- tm_map(clean_crp, removeNumbers)
#matrice
matrice=as.matrix(DocumentTermMatrix(clean_crp))
train_dtm_df=as.data.frame(matrice)
# Ajout colonne de la classe au dataframe
train_dtm_df$topic <- trainSet$topic
train_dtm_df$topic= as.factor(train_dtm_df$topic)

# Créer un modèle bayésien naïf à partir de l'ensemble d'apprentissage
model <- naiveBayes(topic ~ ., data = train_dtm_df)

model

# Préparation des données de test de la même manière
test_corpus <- Corpus(VectorSource(testSet$headline_text))
clean_crp <- tm_map(test_corpus, content_transformer(tolower))
stopwords(kind="en")
clean_crp <- tm_map(clean_crp, removeWords, stopwords("english"))
clean_crp <- tm_map(clean_crp, removePunctuation)
clean_crp <- tm_map(clean_crp, removeNumbers)

test_dtm <- DocumentTermMatrix(clean_crp,control = list(dictionary = Terms(DocumentTermMatrix(clean_crp))))
test_dtm_df <- as.data.frame(as.matrix(test_dtm))


#Prédiction
predictions <- predict(model, newdata = test_dtm_df)

# Afficher les prédictions
print(predictions)

#Matrice confusion
(Confusion=table(testSet$topic,predictions))
# Accuracy  
sum(diag(Confusion))/sum(Confusion)