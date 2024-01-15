################################################################################
######################### Projet Text Mining ###################################
################################################################################

library(tm)
library(wordcloud)

# Lecture du jeu de données

data = read.csv("abcnews-date-text.csv", header = TRUE, sep = ",")

##################### Partie I : De quoi parlait-on en ... ? ###################

filter = (data['publish_date'] > 20030201) & (data['publish_date'] < 20191231)
data_filtered = data[filter,]

Vdata = VCorpus(VectorSource(data_filtered$headline_text))
