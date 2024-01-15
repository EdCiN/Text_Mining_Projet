############################### Import #########################################
library(NLP)
library("tm")
library(RColorBrewer)
library(wordcloud)
library(readr)
data <- read_csv("abcnews-date-text.csv")
############################### Pre process ###################################
data2=data # data test
#dtype
sapply(data, class)
#Format date
data$publish_date <- as.character(data2$publish_date)
data$publish_date <- as.Date(data2$publish_date, format="%Y%m%d")
#Cleaning


############################## Partie I ##############################
