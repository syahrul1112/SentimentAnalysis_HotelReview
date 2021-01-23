#Muh Syahrul Wajhullah J (123170094)
#Yones Fernando  (123180087)
#Kelas E Praktikum
#Sentiment Analysis Review Hotel Dataset

library(here)
library(wordcloud)
library(vroom)
library(tm) #library untuk penggunaan corpus
library(RTextTools) #library untuk klasifikasi teks otomatis
library(e1071) #library untuk naive bayes
library(dplyr) #library untuk manipulasi dataset di r
library(caret) #library untuk mengetahui model yang dibuat apakah baik atau tidak
library(sentimentr)
library(ggplot2)#library untuk mempresentasikan data numerik
library(tidyverse)

#masukin dataset
hotel <- read.csv(here("Chennai_Hotel_Review.csv"))

#BUILD CORPUS
#corpus utk mengubah text (text mining) ke dalam sebuah document
hotel.corpus <- Corpus(VectorSource(hotel$Review_Text))
hotel.corpus

inspect(hotel.corpus[1:3])

#CLEAN TEXT
clean.hotel.corpus=tm_map(hotel.corpus, tolower) #tolower itu digunakan agar penulisan huruf kapital menjadi huruf kecil
inspect(clean.hotel.corpus[1:3])

clean.hotel.corpus=tm_map(clean.hotel.corpus, removeNumbers)#untuk menghapus angka 
inspect(clean.hotel.corpus[1:3])

clean.hotel.corpus=tm_map(clean.hotel.corpus, removePunctuation) #punctuation digunakan utk hapus tanda baca seperti titik dua, titik dsb
inspect(clean.hotel.corpus[1:3])

clean.hotel.corpus=tm_map(clean.hotel.corpus, removeWords, stopwords()) #menghapus stopwords 
inspect(clean.hotel.corpus[1:3])

clean.hotel.corpus=tm_map(clean.hotel.corpus, stripWhitespace) #digunakan untuk menghapus spasi yang terlihat akibat proses sebelumnya
inspect(clean.hotel.corpus[1:5])

#menyimpan dokumen yang sudah bersih ke csv
databersih <- data.frame(text = unlist(clean.hotel.corpus), stringsAsFactors = FALSE)

#--------------------------------------------------------------------------------------

#hitung sentiment dari data bersih
databersih %>%
  get_sentences()%>%
  sentiment() -> data_bersih_sentiment

#cara deteksi kelompok positive, negatif atau netral
cek_data_bersih <- function(x){
  if(x>0){
    "Positive"
  }else if(x<0){
    "Negative"
  }else{
    "Neutral"
  }
}

sentiment_databersih <- data_bersih_sentiment$sentiment
hasil_data_bersih <- sapply(sentiment_databersih, cek_data_bersih)
review <- data_bersih_sentiment$text
hasil_review_databersih <- data.frame(review=review, sentiment=sentiment_databersih, hasil=hasil_data_bersih)
plot(hasil_review_databersih$hasil)

#naive bayes
sampel_databersih <- sample(nrow(hasil_review_databersih), floor(nrow(hasil_review_databersih)*0.8))
hotel.train.databersih <- hasil_review_databersih[sampel_databersih, ]
hotel.test.databersih <- hasil_review_databersih[-sampel_databersih, ]
naive_bayes_databersih <- naiveBayes(hasil~. ,data=hotel.train.databersih, laplace = 1)
prediksi_databersih<-predict(naive_bayes_databersih,hotel.test.databersih)
confusion_matrix_databersih <- confusionMatrix(table(prediksi_databersih,hotel.test.databersih$hasil))
confusion_matrix_databersih

#--------------------------------------------------------------------------------

#term document matrix untuk nampilin per kata di wordcloud
hotel_tdm <- TermDocumentMatrix(clean.hotel.corpus, control = list(wordlengths=c(4,Inf)))
hotel_tdm
hotel_tdm=as.matrix(hotel_tdm)

termFreq=rowSums(as.matrix(hotel_tdm))
hotel_tdm

termFreqsubset=subset(termFreq, termFreq>=5)
termFreqsubset

tdm_df=data.frame(term=names(termFreqsubset), freq=termFreqsubset)
head(tdm_df,10)

#wordcloud, gatau bener apa ngga
wordcloud(words = tdm_df$term, freq = tdm_df$freq, min.freq = 1, max.words = 150, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))

