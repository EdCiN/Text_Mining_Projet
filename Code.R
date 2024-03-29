################################################################################
######################### Projet Text Mining ###################################
################################################################################

library(tm)
library(wordcloud)
library(coop) # Pour la fonction sparsity

# Lecture du jeu de données

data = read.csv("abcnews-date-text.csv", header = TRUE, sep = ",")

##################### Partie I : De quoi parlait-on en ... ? ###################

# filter = (data['publish_date'] > 20030201) & (data['publish_date'] <= 20191231)
# filter2 = (data['publish_date'] >20080301) & (data['publish_date'] <= 20080831)

filter3 = (data['publish_date'] >20080301) & (data['publish_date'] <= 20080304)
# Données entre 01/02/2003 et 31/12/2019
# data_filtered = data[filter,]

# Données dans l'année 2008
# data_filtered_2008 = data[filter2,]

data_filtered_2008_2 = data[filter3,]
# Conversion en VCorpus
Vdata = VCorpus(VectorSource(data_filtered_2008_2$headline_text))


# Nettoyage du contenu : suppression des stopwords, de la ponctuation et 
# conversion en caractères minuscules

custom_stopwords <- c("is","a","for","in")

cleaned_corpus = tm_map(Vdata, content_transformer(tolower))
cleaned_corpus = tm_map(cleaned_corpus,removePunctuation)
cleaned_corpus = tm_map(cleaned_corpus, removeNumbers)
cleaned_corpus = tm_map(cleaned_corpus, removeWords, stopwords('english'))
cleaned_corpus = tm_map(cleaned_corpus, removeWords, custom_stopwords)

# Pour vérifier que le nettoyage a été appliqué
# content(cleaned_corpus[[1]])
# content(cleaned_corpus[[2]])

# Transformation en bag of words
dtm <- DocumentTermMatrix(cleaned_corpus)
matrix <- as.matrix(dtm)

# dim(matrix) # 40554 documents * 19978 mots dans le vocabulaire: 

# sparsity(matrix) # 0.9997488

frequency <- colSums(matrix)
frequency <-sort(frequency, decreasing = TRUE)
head(frequency)

# words <- names(frequency)
# wordcloud(words[1:100], frequency[1:100], color = brewer.pal(8,"Dark2"))

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


############### Partie III : Définir des clusters de dépêches ##################

### Définition des fonctions pour l'algorithme Frequent Term Sets

# Fonction Cov
coverage <- function(dtm, termSets){
  # dtm est un DocumentTermMatrix
  # termSets est une concaténation de mots. Par exemple : c("banane","pomme")
  # Renvoie la liste des documents de dtm contenant les mots de termSet
  
  if(length(termSets) == 0){return(c())}
  else{
    listDocs = c()
    i = 588
    for(i in as.integer(dtm$dimnames$Docs)){
      positionList = which(dtm$i == i)
      termsOfDocIndexList = dtm$j[positionList]
      docTerms = dtm$dimnames$Terms[termsOfDocIndexList]
    
      if (all(termSets %in% docTerms)){
        listDocs <-c(listDocs, i)
      }
    }
  }
  return(listDocs)
}

# Standard overlap
standardOverlap <- function(frequentTermSet, cluster, dtm){
  # frequentTermSet est une chaîne de caractères
  # cluster est une concaténation d'entiers : c(1,5,8), les chiffres
  # désignent le numéro du document dans dtm, appartenant au cluster
  # dtm est un DocumentTermMatrix
  if(is.null(cluster)){return(Inf)}
  value = 0
  for(doc in cluster){
    # Pour obtenir la liste des mots du doc
    positionList = which(dtm$i == doc)
    termsOfDocIndexList = dtm$j[positionList]
    docTerms = dtm$dimnames$Terms[termsOfDocIndexList]
    
    # Compter le nombre de frequent terms dans chaque doc
    frequentTermInDocBoolList = frequentTermSet %in% docTerms 
    numFreqTermsInDoc = length(frequentTermInDocBoolList[
      frequentTermInDocBoolList == TRUE
    ])
    value <- value + numFreqTermsInDoc - 1
  }
  result = value/length(cluster)
  return(result)
}

# Fonction de suppression de documents dans le DTM
removeFromDTM <- function(bestCandidate, dtm){
  cov <- coverage(dtm, bestCandidate)
  
  # Si la couverture est nulle, il n'y a rien à faire
  if(is.null(cov)){return(dtm)}
  for(i in cov){
    listIndexToRemove = which(dtm$i == i)
    dtm$dimnames$Docs = dtm$dimnames$Docs[
      -which(dtm$dimnames$Docs == as.character(i))
      ]
    
    dtm$i = dtm$i[-listIndexToRemove]
    dtm$j = dtm$j[-listIndexToRemove]
    dtm$v = dtm$v[-listIndexToRemove]
  }
  
  dtm$nrow = dtm$nrow - length(cov)
  
  return(dtm)
}

# Fonction de suppression des documents qui ne contiennent aucun frequent term
earlyDTMClean = function(frequentTermSet, dtm){
  cov <- c()
  for(term in frequentTermSet){
    cov <- c(cov, coverage(dtm, term))
  }
  cov <- unique(cov)
  list_docs_to_remove = c()
  for(i in dtm$dimnames$Docs){
    if(!(i %in% cov)){
      list_docs_to_remove <-c(list_docs_to_remove, i)
    }
  }
  dtm$dimnames$Docs = dtm$dimnames$Docs[-as.integer(list_docs_to_remove)]
  
  for(i in list_docs_to_remove){
    positionList = which(dtm$i == i)
    
    dtm$i = dtm$i[-positionList]
    dtm$j = dtm$j[-positionList]
    dtm$v = dtm$v[-positionList]
    
  }
  dtm$nrow = dtm$nrow - length(list_docs_to_remove)
  
  return(dtm)
}


# Algorithme principal
selectTermSets <- function(dtm, minsup = 15){
  selectedTermSets = c()
  listCoverages = c()
  n = length(dtm$dimnames$Docs)
  
  # Renvoie les termes apparaissant au moins minsup fois dans le dtm
  remainingTermSet = findFreqTerms(dtm,lowfreq = minsup)
  cov = c()
  for(term in remainingTermSet){
    cov <- c(cov, coverage(dtm, term))
  }
  cov <- unique(cov)
  dtm = earlyDTMClean(remainingTermSet, dtm)
  # Tant qu'il y a des candidats à ajouter et que la couverture des 
  # frequent term n'est pas entière
  while(length(remainingTermSet) > 0 & length(coverage(dtm, selectedTermSets))!=n){
    listOverlaps = c()
    termSet = remainingTermSet[[1]]
    for(termSet in remainingTermSet){
      cluster = coverage(dtm, termSet)
      listOverlaps <- c(listOverlaps, standardOverlap(remainingTermSet,
                                                      cluster,
                                                      dtm))
    }
    # Selection du candidat contribuant à un overlap minimal
    bestCandidateIndex = which.min(listOverlaps)
    bestWords = remainingTermSet[[bestCandidateIndex]]
    selectedTermSets <- c(selectedTermSets, 
                          list(bestWords))
    listCoverages = c(listCoverages, 
                      list(coverage(dtm, bestWords)))
    
    # Suppression de la couverture du meilleur mot du dtm
    dtm = removeFromDTM(bestWords, dtm)
    list_sets_to_remove = c()
    for(i in 1:length(remainingTermSet)){
      if(all(bestWords %in% remainingTermSet[[i]])){
        list_sets_to_remove = c(list_sets_to_remove,i)
      }
    }
    remainingTermSet = remainingTermSet[-list_sets_to_remove]
  }
  
  print(paste("Couverture entière des documents : ", length(dtm$dimnames$Docs)==0))
  print(paste("Documents n'appartenant à aucun cluster : ", 
              intersect(as.character(cov), dtm$dimnames$Docs)))
  
  return(mapply(c,selectedTermSets, listCoverages))
}
findFreqTerms(dtm,15)
x = selectTermSets(dtm,15)


