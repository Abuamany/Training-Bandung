library(shiny)
library(rpart)
library(rpart.plot)
library(psych)
library(caret)
library(ROCR)
library(MLmetrics)

server <- shinyServer(function(input, output) {
  
  data <- reactive({
    inFile <- input$file1
    req(inFile)
    data <- read.csv(inFile$datapath, header=input$header, sep=input$sep,  quote=input$quote)
    data
  })
  
  # reactive test data
  testdata <- reactive({
    inFile <- input$file2
    req(inFile)
    read.csv(inFile$datapath)
  })
  
  bs <- reactive({
    inFile <- input$file1
    req(inFile)
    x <- read.csv(inFile$datapath, header=input$header, sep=input$sep,  quote=input$quote)
    describe(x, skew=FALSE, ranges=FALSE)
  })
  
  
  model <- reactive({
    req(input$text %in% names(data() ))
    depVar <- input$text
    mod <- as.formula(paste0(depVar, " ~ ."))
    modrpart <- rpart(mod, data = data() )
    modrpart
  })
  
  # Show the first "n" observations
  output$view <- renderTable({
    head(data())
  })
  
  output$textarea.out <- renderPrint({
    bs()
  })
  
  output$text1 <- renderPrint({ # changed to renderPrint
    model()
  })
  
  output$plot <- renderPlot({
    rpart.plot(model() )
  })

  output$view1 <- renderTable({
    head(testdata())
  })
    
  # print predictions to the console
  observe({ 
    pred <- predict(object = model(), newdata = testdata(), type="class" )
    print(head(pred, 10))
    inFile <- input$file2
    req(inFile)
    y<-read.csv(inFile$datapath)
    ytest<-y$Hasil
    her<-confusionMatrix(ytest,pred)
    print(confusionMatrix(ytest,pred))
  })
  
    observe({ 
    pred <- predict(object = model(), newdata = testdata())
    write.csv(pred, file = "MyData.csv")
    })  
    
    pred <- reactive({
      pred <- predict(object = model(), newdata = testdata() )
      print(head(pred, 10))
      })
    
    
    output$view4 <- renderTable({
      head(pred())
      })
    
    output$view5 <- renderPrint({
      pred <- predict(object = model(), newdata = testdata(), type="class" )
      inFile <- input$file2
      req(inFile)
      y<-read.csv(inFile$datapath)
      ytest<-y$Hasil
      print(confusionMatrix(ytest,pred))
    })
    
    output$plot1 <- renderPlot({
      pred <- predict(object = model(), newdata = testdata(), type="prob" )
      inFile <- input$file2
      req(inFile)
      y<-read.csv(inFile$datapath)
      ytest<-y$Hasil
      predroc <- prediction(predictions = pred[,2], labels=ytest)
      roc<-performance(predroc,"tpr", "fpr")
      plot(roc, colorize=TRUE, main="ROC Curve", xlab="Sensitivity", ylab="1-Specifity")
      abline(a=0, b=1, col="blue")
    })

    output$testHTML <- renderText({
      pred <- predict(object = model(), newdata = testdata(), type="prob" )
      inFile <- input$file2
      req(inFile)
      y<-read.csv(inFile$datapath)
      ytest<-y$Hasil
      predroc <- prediction(predictions = pred[,2], labels=ytest)
      auc<-performance(predroc,"auc")
      auc<-unlist(slot(auc, "y.values"))
      pred1 <- predict(object = model(), newdata = testdata(), type="class" )
      F1_score<- F1_Score(ytest, pred1, positive = NULL)
      paste("Akurasi berdasarkan AUC-ROC:",round(auc,4), "<br>", "F-1 Score:",F1_score)
    })
    
    output$HTML2 <- renderText({
    paste("<b>","Program Ini Dibuat oleh Heru Wiryanto","</b>", "Konsultan Pengembangan Sumberdaya Manusia dengan bidang kekhususan; Menerapkan lansekap arsitektur pembelajaran melalui Implementasi Corporate University, Membangun Tranformasi Kelembagaan melalui penerapan nilai organisasi sehat dan berkinerja tinggi, Mengembangkan konsep HR analytics dengan berbasis Positive Measurement.","Selama 25 tahun meniti pengalaman hidupnya yang didukung oleh minatnya yang tinggi dibidang Pengukuran yang obyektif pada Manajemen Sumber daya manusia mendorong lulusan Fakultas Psikologi Universitas Padjadjaran, dan Pasca Sarjana Fakultas Psikologi Universitas Indonesia ini untuk berinovasi dan melakukan adopsi konsep dan metode termuktahir dalam dunia bisnis seperti penerapan metode Structural Equation Modeling,  RASCH Model dan Item Response Theory, Bayesian Statistics sehingga lahirlah konsep-konsep seperti : Model Kepemimpinan Terintegrasi, Model Organisasi Sehat dan Berkinerja Tinggi, Model Berfikir Komputasi, Model Evaluasi Kebijakan yang teruji secara obyektif melalui data pendukung yang bersifat empiris.", "Saat ini diberikan amanah menjadi narasumber untuk membantu Institusi Pemerintah, BUMN, swasta seperti : Garuda Indonesia, Telkom Indonesia, PLN, Kementerian Kesehatan RI, Kementerian Keuangan RI, BTPN, dlsbnya.") 
    
  })
    
})