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


