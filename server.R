library(shiny)
library(textreuse)
library(magrittr)
library(DT)

shinyServer(function(input, output) {
  
  data <- reactive({
    # Read .csv-file from the HTML input
    inFile <- input$file
    
    if (is.null(inFile))
      return(NULL)
    
    data <- read.csv(inFile$datapath, encoding = "UTF-8")
    
    # Remove any existing files
    dir.create(file.path(getwd(), "files"))
    do.call(file.remove, list(list.files("files/", full.names = TRUE)))
    
    # Create .txt-files for every column
    w_file <- function(l) {
      f <- file(paste("files/", l["Node.ID"], ".txt", sep = ""), open = "w")
      write(l["Transcription"], file = f)
      close(f)
    }
    
    apply(data, 1, w_file)
    
    # Rename columns and remove other columns
    names(data)[names(data) == 'Node.ID'] <- 'id'
    names(data)[names(data) == 'Title'] <- 'title'
    names(data)[names(data) == 'Transcription'] <- 'transcription'
    data <- data[c("id", "title", "transcription")]
    
    return(data)
  })
  
  corpus <- reactive({
    # Return NULL if there is no input
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)

    TextReuseCorpus(dir = "files", tokenizer = tokenize_ngrams, n = 5)
  })
  
  most_pc <- reactive({
    # Return NULL if there is no input
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    # Create a corpus and do the pairwise comparisons
    comparisons <- pairwise_compare(corpus(), jaccard_similarity)
    pc <- pairwise_candidates(comparisons)
    
    # Cut off at 0.001, and display the 100 highest scores
    pc <- subset(pc, pc$score > 0.001) 
    head(pc[order(-pc$score),], 100)
  })

  output$table <- DT::renderDataTable({
    # Return NULL if there is no input
    inFile <- input$file
    if (is.null(inFile))
      return(invisible(NULL))
    
    # Retrieve the title from the data
    data <- data()
    names(data)[names(data) == 'title'] <- 'a_title'
    total <- merge(most_pc(), data, by.x = "a", by.y = "id")
    names(data)[names(data) == 'a_title'] <- 'b_title'
    total <- merge(total, data, by.x = "b", by.y = "id")
    
    # Create a link to the node
    createLink <- function(id, name) {
      sprintf('<a href="http://artechne.hum.uu.nl/node/%s" target="_blank">%s</a>', id, name)
    }

    total <- within(total, "A" <- createLink(a, a_title))
    total <- within(total, "B" <- createLink(b, b_title))
    
    # Remove unnecessary columns and order by score
    total <- total[c("A", "B", "score")]
    total <- total[order(-total$score),]
    
    return(DT::datatable(total, selection = 'single', escape = FALSE, rownames = FALSE) %>% formatRound('score', digits = 4))
  })
  
  output$text <- renderPrint({
    # Retrieve the last clicked table row
    c <- input$table_row_last_clicked

    if (is.null(c))
      return(invisible(NULL))

    # Output the longest matching alignment
    print(tags$h3("Longest matching alignment"))

    a <- most_pc()[c, 1]
    b <- most_pc()[c, 2]
    alignment <- align_local(corpus()[[a]], corpus()[[b]])
    
    print(tags$pre(alignment$a_edit))
    print(tags$pre(alignment$b_edit))
  })
  
  output$showA <- renderPrint({
    # Retrieve the last clicked table row
    c <- input$table_row_last_clicked
    
    if (is.null(c))
      return(invisible(NULL))
    
    # Find the matching data point and output the title/transcription
    match <- which(data() == most_pc()[c, 1])
    print(h3("A"))
    print(h4(data()[match, ]$title))
    print(p(data()[match, ]$transcription))
  })
  
  output$showB <- renderPrint({
    # Retrieve the last clicked table row
    c <- input$table_row_last_clicked
    
    if (is.null(c))
      return(invisible(NULL))
    
    # Find the matching data point and output the title/transcription
    match <- which(data() == most_pc()[c, 2])
    print(h3("B"))
    print(h4(data()[match, ]$title))
    print(p(data()[match, ]$transcription))
  })
  
})
