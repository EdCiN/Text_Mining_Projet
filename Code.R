################################################################################
######################### Projet Text Mining ###################################
################################################################################

library(tm)
library(wordcloud)
library(coop)

# Lecture du jeu de données

data = read.csv("abcnews-date-text.csv", header = TRUE, sep = ",")

##################### Partie I : De quoi parlait-on en ... ? ###################

filter = (data['publish_date'] > 20030201) & (data['publish_date'] <= 20191231)
filter2 = (data['publish_date'] >20070801) & (data['publish_date'] <= 20071231)

# Données entre 01/02/2003 et 31/12/2019
data_filtered = data[filter,]

# Données dans l'année 2008
data_filtered_2008 = data[filter2,]

# Conversion en VCorpus
Vdata = VCorpus(VectorSource(data_filtered_2008$headline_text))


# Nettoyage du contenu : suppression des stopwords, de la ponctuation et 
# conversion en caractères minuscules

custom_stopwords <- c("is","a","for","in")

cleaned_corpus = tm_map(Vdata, content_transformer(tolower))
cleaned_corpus = tm_map(cleaned_corpus,removePunctuation)
cleaned_corpus = tm_map(cleaned_corpus, removeNumbers)
cleaned_corpus = tm_map(cleaned_corpus, removeWords, stopwords('english'))
cleaned_corpus = tm_map(cleaned_corpus, removeWords, custom_stopwords)

# Pour vérifier que le nettoyage a été appliqué
content(cleaned_corpus[[1]])
content(cleaned_corpus[[2]])

# Transformation en bag of words
dtm <- DocumentTermMatrix(cleaned_corpus)
matrix <- as.matrix(dtm)

dim(matrix) # 32640 documents * 17473 mots dans le vocabulaire: 

sparsity(matrix) # 0.9997128

frequency <- colSums(matrix)
frequency <-sort(frequency, decreasing = TRUE)
head(frequency)

words <- names(frequency)
wordcloud(words[1:100], frequency[1:100], color = brewer.pal(8,"Dark2"))

