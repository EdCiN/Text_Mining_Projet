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

library(dplyr)

library(stringr)

# Ajouter une nouvelle colonne qui indique le sujet de chaque dépêche
df <- data %>%
  mutate(topic = case_when(
    str_detect(headline_text, "\\biraq\\b") ~ "Iraq", 
    str_detect(headline_text, "\\bcovid\\b") ~ "Covid",
    str_detect(headline_text, "\\bclimate\\b") ~ "Climate",
    TRUE ~ "Other"
  ))

# Supprimer les lignes qui contiennent le sujet "Other"
df <- df %>%
  filter(topic != "Other")

#
library(tm)
library(wordcloud)

# Supposons que 'data' est votre dataframe et qu'il a deux colonnes : 'text' et 'topic'

# Divisez votre dataframe par sujet
data_split <- split(df$headline_text, df$topic)

# Créez une fonction pour nettoyer et calculer les fréquences de mots
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

# Calculez les fréquences de mots pour chaque sujet
frequencies <- lapply(data_split, get_word_frequencies)

# Créez une wordcloud pour chaque sujet
for (topic in names(frequencies)) {
  frequency <- frequencies[[topic]]
  words <- names(frequency)
  wordcloud(words[1:100], frequency[1:100], main = paste("Wordcloud for topic:", topic))
}





### Proportion
# Calculer le nombre de dépêches pour chaque sujet
class_counts <- table(df$topic)

# Calculer la proportion de dépêches pour chaque sujet
class_proportions <- class_counts / nrow(df)

# Afficher les proportions
print(class_proportions)
### Sport est moins présent que les autres classes nous allons faire un train/test equilibre


df <- df[ , -which(names(df) %in% "publish_date")]
###### Train/Test
# Charger le package
library(tibble)


library(caret)



set.seed(25) # pour la reproductibilité
trainIndex <- createDataPartition(df$topic, p = .8, 
                                  list = FALSE, 
                                  times = 1, 
                        
                                  )

trainSet <- df[trainIndex, ]
testSet  <- df[-trainIndex, ]

##verif proportion
# Calculer le nombre de dépêches pour chaque sujet dans l'ensemble d'apprentissage
train_class_counts <- table(trainSet$topic)

# Calculer la proportion de dépêches pour chaque sujet dans l'ensemble d'apprentissage
train_class_proportions <- train_class_counts / nrow(trainSet)

# Afficher les proportions pour l'ensemble d'apprentissage
print(train_class_proportions)

# Calculer le nombre de dépêches pour chaque sujet dans l'ensemble de test
test_class_counts <- table(testSet$topic)

# Calculer la proportion de dépêches pour chaque sujet dans l'ensemble de test
test_class_proportions <- test_class_counts / nrow(testSet)

# Afficher les proportions pour l'ensemble de test
print(test_class_proportions)

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
?tm_map()
#matrice
matrice=as.matrix(DocumentTermMatrix(clean_crp))
train_dtm_df=as.data.frame(matrice)
# Ajouter la colonne de classe au dataframe
train_dtm_df$topic <- trainSet$topic
train_dtm_df$topic= as.factor(train_dtm_df$topic)
# Créer un modèle bayésien naïf à partir de l'ensemble d'apprentissage

model <- naiveBayes(topic ~ ., data = train_dtm_df)

model

# Préparer les données de test de la même manière
test_corpus <- Corpus(VectorSource(testSet$headline_text))
clean_crp <- tm_map(test_corpus, content_transformer(tolower))
stopwords(kind="en")
clean_crp <- tm_map(clean_crp, removeWords, stopwords("english"))
clean_crp <- tm_map(clean_crp, removePunctuation)
clean_crp <- tm_map(clean_crp, removeNumbers)

test_dtm <- DocumentTermMatrix(clean_crp,control = list(dictionary = Terms(DocumentTermMatrix(clean_crp))))
test_dtm_df <- as.data.frame(as.matrix(test_dtm))
?predict
#Prédire les classes pour l'ensemble de test
predictions <- predict(model, newdata = test_dtm_df)

# Afficher les prédictions
print(predictions)

?naiveBayes
#Matrice confusion
test_dtm_df$topic <- testSet$topic
test_dtm_df$topic= as.factor(test_dtm_df$topic)
test_dtm_df_verif2=test_dtm_df
test_dtm_df_verif2$predictions=predictions


test_dtm_df_verif=cbind(test_dtm_df,predictions)
Confusion=table(test_dtm_df_verif$topic,predictions)
Confusion
# Accuracy  
sum(diag(Confusion))/sum(Confusion)
