
library(rsconnect)
library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(shinythemes)
memory.profile()
# The list of valid books
books <<- list("A Mid Summer Night's Dream" = "summer",
               "Pride and Prejudice" = "Prejudice",
               "Romeo and Juliet" = "romeo")

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(book) {
  # Careful not to let just any name slip in here; A 
  # malicious user could manipulate this value.
  if (!(book %in% books))
    stop("Unknown book")
  
  text <- readLines(sprintf("./%s.txt.gz", book),
                    encoding ="UTF-8")
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy","thou", "thee", "the", "and", "but"))
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 2))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})
ui <- fluidPage(
  theme = shinytheme("slate"),
  # Application title
  titlePanel("Twórz mapę myśli z ulubionych książek"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Wybierz książkę:",
                  choices = books),
      actionButton("update", "Zmień"),
      hr(),
      sliderInput("freq",
                  "Minimalna częstotliwość:",
                  min = 1,  max = 50, value = 15),
      sliderInput("max",
                  "Maksymalna liczba słów:",
                  min = 1,  max = 300,  value = 100)
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot")
    )
  )
)
server <- function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(5,0.8),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
}
shinyApp(ui, server) 

