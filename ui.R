shinyUI(pageWithSidebar(
  #--judul shiny
  headerPanel("Sentiment Analysis Review Hotel With Naive Bayes Method"),
  
  sidebarPanel(
    #Selector for file upload
    fileInput('datafile', 'Choose CSV file (maximum 5 MB)',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    #download csv
    tags$div(class="header", checked=NA,
             tags$p("This shiny app uses Dataset Hotel Chennai_Hotel_Review that collected from Kaggle.com datasets and the process uses a Review_Text coloumn"),
             tags$a(href="https://drive.google.com/file/d/134lDr0lJDUN0Kq2ZIatbnn2BR8MAA19C/view?usp=sharing", "Please, Download the data.")),
    hr(),
    sliderInput("freq",
                "Minimum Frequency:",
                min = 1,  max = 50, value = 15),
    
    sliderInput("max",
                "Maximum Number of Words:",
                min = 1,  max = 300,  value = 100),
    hr(),
    #The action button prevents an action firing before we're ready
    actionButton("proses", "Analyze"),
    hr(),
    actionButton("plot","Wordcloud"),
    hr(),
    actionButton("algorithm","Naive Bayes"),
    hr()
    
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Dataset",h4("Dataset"),DT::dataTableOutput("filetable")),
      tabPanel("Analyze",h4("Analyze Result From Sentiment Calculation"),DT::dataTableOutput("finaltable"),plotOutput("final")),
      tabPanel("Wordcloud", h4("Wordcloud Plot"),plotOutput("plotwordcloud")),
      tabPanel("Naive Bayes", h4("The Result of Predict Naive Bayes"),DT::dataTableOutput("bayes"),plotOutput("plotbayes"),imageOutput("myImage"))
    )
    #tableOutput("filetable"),
    #tableOutput("finaltable"),
  )
))