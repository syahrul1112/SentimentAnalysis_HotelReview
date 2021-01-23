library(shiny)
library(ggmap)
library(tm)
library(sentimentr)
library(e1071) #library untuk naive bayes
library(caret) #library untuk mengetahui model yang dibuat apakah baik atau tidak
library(ggplot2)
library(wordcloud)
library(memoise)
library(DT)
library(vroom)
library(here)
library(png)

getTermMatrix <- memoise(function(review){
  hotel.corpus <- Corpus(VectorSource(review))
  clean.hotel.corpus=tm_map(hotel.corpus, tolower) #tolower itu digunakan agar penulisan huruf kapital menjadi huruf kecil
  clean.hotel.corpus=tm_map(clean.hotel.corpus, removeNumbers)#untuk menghapus angka 
  clean.hotel.corpus=tm_map(clean.hotel.corpus, removePunctuation) #punctuation digunakan utk hapus tanda baca seperti titik dua, titik dsb
  clean.hotel.corpus=tm_map(clean.hotel.corpus, removeWords, stopwords()) #menghapus stopwords 
  clean.hotel.corpus=tm_map(clean.hotel.corpus, stripWhitespace) #digunakan untuk menghapus spasi yang terlihat akibat proses sebelumnya
  hotel_tdm <- TermDocumentMatrix(clean.hotel.corpus, control = list(minWordLength=1))
  m=as.matrix(hotel_tdm)
  sort(rowSums(m), decreasing = TRUE)
})

