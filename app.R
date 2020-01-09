library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(gutenbergr)
library(shinythemes)
library(wordcloud)
library(tm)
library(memoise)
library(tidytext)
library(stringr)
library(corpus)
library(shinyWidgets)
library(tidyr)

#############################GATHERING AND PREPROCESSING OF DATA#############################################################################

gutenberg_works(str_detect(author, "Brontë"))[2] %>% na.omit() -> titles  ## select the works of authors with name Bronte
## titles

getTermMatrix <- memoise(function(book) {
text <- read_lines(toString(book))
myCorpus = Corpus(VectorSource(text))
myCorpus = tm_map(myCorpus, content_transformer(tolower))   ## convert all words to lower case
myCorpus = tm_map(myCorpus, removePunctuation)              ## remove all punctuations
myCorpus = tm_map(myCorpus, removeNumbers)                  ## remove all the numbers
myCorpus = tm_map(myCorpus, removeWords, c(stopwords("SMART"), "thy", "thou", "thee"))  ## remove common stop words such as a, an, the etc.
myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))                 ## convert into a matrix of words and their frequency
m = as.matrix(myDTM)
sort(rowSums(m), decreasing = TRUE)                                                     ## highest frequency at the top

})


################################USER INTERFACE################################################################################################

ui <- fluidPage(
    theme = shinytheme("cerulean"),
    setBackgroundImage(src = "bronte.jpg"),
    navbarPage(
        "Brontë Word Cloud",
        tabPanel("An Interactive Web Application", icon = icon("Braille"),
                 sidebarLayout(
                     sidebarPanel(
                         selectizeInput("gutenberg_work", "Choose a book by the Brontës", titles,
                                        selected = "Jane Eyre: An Autobiography",
                                        multiple = FALSE, options = NULL),
                         sliderInput("freq", "Word should occur at least this many times in the book:",
                                     min = 1, max = 100, value = 50),
                         sliderInput("max", "How many words do you want to see in the Word Cloud?:",
                                     min = 1, max = 500, value = 50)
                         
                     ),
                     mainPanel(plotOutput("plot"), width = 3)
                 )
        
        ),
        tabPanel("Contact Me", icon = icon("address-card"),
                 h5(tags$ul(tags$li(a("Send me an e-mail", href = "mailto:abhi.dangol@gmail.com", target = "blank")))),
                 h5(tags$ul(tags$li(a("My Github Page", href = "https://github.com/abhishek-dangol", target = "blank"))))
        
    )

    )
)


##############################SERVER###########################################################################################################
server <- function(input, output, session){
    terms <- reactive({
        id <- gutenberg_works(title == input$gutenberg_work)[1]
        gutenberg_download(id)[2] %>%
            unnest_tokens(word, text) -> gutenberg_work
        
        isolate({withProgress({
            setProgress(message = "Loading...")
            getTermMatrix(gutenberg_work)})
            
        })
    })
    output$plot <- renderPlot({
        v <- terms()
        wordcloud(names(v), v, scale = c(4, 0.5), random.order = F, rot.per = 0.30, use.r.layout = F,
                  min.freq = input$freq, max.words = input$max,
                  colors = brewer.pal(8, "Dark2"))
    })
    
}

shinyApp(ui = ui, server = server)
    

