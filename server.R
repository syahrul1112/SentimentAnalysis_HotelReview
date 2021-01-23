
shinyServer(function(input, output, session) {
  
  #select file
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  #This previews the CSV data file
  output$filetable <- DT::renderDataTable({
    filedata()
  })
  
  #This function is the one that is triggered when the action button is pressed
  #The function is a geocoder from the ggmap package that uses Google maps geocoder to geocode selected locations
  analyze <- reactive({
    if (input$proses == 0) return(NULL)
    df=filedata()
    if (is.null(df)) return(NULL)
    
    isolate({
      #Get the CSV file data
      dummy=filedata()
      hotel.corpus <- Corpus(VectorSource(dummy$Review_Text))
      
      clean.hotel.corpus=tm_map(hotel.corpus, tolower) #tolower itu digunakan agar penulisan huruf kapital menjadi huruf kecil
      clean.hotel.corpus=tm_map(clean.hotel.corpus, removeNumbers)#untuk menghapus angka 
      clean.hotel.corpus=tm_map(clean.hotel.corpus, removePunctuation) #punctuation digunakan utk hapus tanda baca seperti titik dua, titik dsb
      clean.hotel.corpus=tm_map(clean.hotel.corpus, removeWords, stopwords("en")) #menghapus stopwords 
      clean.hotel.corpus=tm_map(clean.hotel.corpus, stripWhitespace) #digunakan untuk menghapus spasi yang terlihat akibat proses sebelumnya
      databersih <- data.frame(text = unlist(clean.hotel.corpus), stringsAsFactors = FALSE)
      #biar bisa ngitung sentiment di setiap kalimat. Jadi sebuah kalimat itu di itung per kata, ada berapa kata terus diitung sentimentnya pake fungsi sentiment() yang ada di library sentimentr. 
      databersih %>%
        get_sentences()%>%
        sentiment() -> data_bersih_sentiment
      #ngelompokkin, kalau hasil lebih dari 0 dia positif kalau kurang 0 negatif kalau diantara itu berarti netral
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
      hasil_review_databersih <- data.frame(hasil=hasil_data_bersih)
      
    })
  })
  
  #-------------------------------------------------------------------------
  #This function is the one that is triggered when the action button is pressed
  #The function is a geocoder from the ggmap package that uses Google maps geocoder to geocode selected locations
  bayes <- reactive({
    if (input$algorithm == 0) return(NULL)
    df=filedata()
    if (is.null(df)) return(NULL)
    
    isolate({
      #Get the CSV file data
      dummy=filedata()
      hotel.corpus <- Corpus(VectorSource(dummy$Review_Text))
      clean.hotel.corpus=tm_map(hotel.corpus, tolower) #tolower itu digunakan agar penulisan huruf kapital menjadi huruf kecil
      clean.hotel.corpus=tm_map(clean.hotel.corpus, removeNumbers)#untuk menghapus angka 
      clean.hotel.corpus=tm_map(clean.hotel.corpus, removePunctuation) #punctuation digunakan utk hapus tanda baca seperti titik dua, titik dsb
      clean.hotel.corpus=tm_map(clean.hotel.corpus, removeWords, stopwords("en")) #menghapus stopwords 
      clean.hotel.corpus=tm_map(clean.hotel.corpus, stripWhitespace) #digunakan untuk menghapus spasi yang terlihat akibat proses sebelumnya
      databersih <- data.frame(text = unlist(clean.hotel.corpus), stringsAsFactors = FALSE)
  #biar bisa ngitung sentiment di setiap kalimat. Jadi sebuah kalimat itu di itung per kata, ada berapa kata terus diitung sentimentnya pake fungsi sentiment() yang ada di library sentimentr. 
      databersih %>%
        get_sentences()%>%
        sentiment() -> data_bersih_sentiment
  #ngelompokkin, kalau hasil lebih dari 0 dia positif kalau kurang 0 negatif kalau diantara itu berarti netral
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
      
      #------------------------------------mulai bayes
      sampel_databersih <- sample(nrow(hasil_review_databersih), floor(nrow(hasil_review_databersih)*0.8))
      hotel.train.databersih <- hasil_review_databersih[sampel_databersih, ] 
      hotel.test.databersih <- hasil_review_databersih[-sampel_databersih, ]
      
      naive_bayes_databersih <- naiveBayes(hasil~., data=hotel.train.databersih)#actual
      prediksi_databersih<-as.data.frame(predict(naive_bayes_databersih,hotel.test.databersih))#predict
      
    })
  })
  
  #---------------------------------------------------------------------------------
  #This function is the one that is triggered when the action button is pressed
  #The function is a geocoder from the ggmap package that uses Google maps geocoder to geocode selected locations
  getwordcloud <- reactive({
    
    if (input$plot == 0) return(NULL)
    df=filedata()
    if (is.null(df)) return(NULL)
    
    isolate({
        dummy=filedata()
        getTermMatrix(dummy$Review_Text)
    })
        
  })
  #-------------------------------------------------------------------------
  output$finaltable <- DT::renderDataTable({
    if (input$proses == 0) return(NULL)
    analyze()
  })
  output$final <- renderPlot({
    if (input$proses == 0) return(NULL)
    v <- analyze()
    plot(v)
  })
  output$bayes <- DT::renderDataTable({
    if (input$algorithm == 0) return(NULL)
    bayes()
    #confusionMatrix(table(prediksi_databersih,hotel.test.databersih))
  })
  output$plotbayes <- renderPlot({
    if (input$algorithm == 0) return(NULL)
    v <- bayes()
    plot(v)
  })
  wordcloud_rep <- repeatable(wordcloud)
  output$plotwordcloud <- renderPlot({
    if (input$plot == 0) return(NULL)
    v<-getwordcloud()
    wordcloud_rep(names(v), v, scale=c(3,.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  output$myImage <- renderImage({
    if (input$algorithm == 0) return(NULL)
    else
    {
    # Return a list containing the filename
    list(
         #src = "C:/Shiny/shiny hotel/akurasi.png",
         src = "image/akurasi.png",
         contentType = 'image/png',
         width = 450,
         height = 350,
         alt = "Confusion Matrix")}
    
  }, deleteFile = FALSE) #--buat hapus gmbr kalau sudah keoutput
})