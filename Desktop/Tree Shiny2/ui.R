library(shiny)
library(shinythemes)
  shinyUI(fluidPage(theme = shinytheme("slate"),
  headerPanel("ALGORITHMIC ASSESSMENT MENGGUNAKAN C4.5"),
  
  
  sidebarPanel(
      fileInput('file1', 'Memuat File Training',accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"' ,
                     'Single Quote'="'") ,
                   '"') ,
      
      br(),
      textInput("text", label = strong("Target Prediksi"), 
                value = "Masukan Variabel Target Prediksi"),
      
      # new fileInput
      br(),
      fileInput('file2', 'Memuat Data Test', 
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv'))
      
    ),
      
    mainPanel(
        tabsetPanel(
          tabPanel("Fitur Data/Prediktor",tableOutput("view")), 
          tabPanel("Informasi Statistika Dasar",verbatimTextOutput("textarea.out")), 
          tabPanel("Algoritma Model", textOutput("text1")),
          tabPanel("Diagram Keputusan", plotOutput("plot")),
          tabPanel("Data Baru yang diPredik", tableOutput("view1")),
          tabPanel("Hasil Prediksi",tableOutput("view4")),
          tabPanel("Matrik Konfusi", verbatimTextOutput("view5")),
          tabPanel("ROC-Analysis", plotOutput("plot1"),htmlOutput("testHTML")),
          tabPanel("Tentang Kami", htmlOutput("HTML2"))
        )
  )
))