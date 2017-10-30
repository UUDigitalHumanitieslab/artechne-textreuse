library(shiny)
library(DT)

shinyUI(fluidPage(
  fluidRow(
    fluidRow(
      column(width=12, tags$div(class = "alert alert-info", checked = NA,
                                tags$p("Text reuse allows you to upload a .csv-file containing the data from a set records, and analyze if any text was reused.",
                                       "This can help you understand whether there are connections between two records, e.g. whether an author copied something from an earlier source."),
                                tags$p("The values in the 'score' column are Jaccard similarities and range between 0 and 1 (cut-off at 0.001). A higher score means more overlap between texts.",
                                       "Clicking on a row allows to investigate the match.")
      ))
    ),
    fluidRow(
      column(width=12, titlePanel("ARTECHNE - text reuse calculation")),
      column(width=12, wellPanel(fileInput("file", "Upload your exported file here (.csv-format)",
                                           accept = c(
                                             "text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")))),
      column(width=12, hr())
    ),
    fluidRow(
      column(width=12, DT::dataTableOutput("table"))
    ),
    fluidRow(
      column(width=12, htmlOutput("text"))
    ),
    fluidRow(
      column(width=5, htmlOutput("showA")),
      column(width=5, htmlOutput("showB"))
    )
  )
))
