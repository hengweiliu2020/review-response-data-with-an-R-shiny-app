#app.R
#Shiny App for Response Data
options(shiny.maxRequestSize = 30*1024^2)
library(haven)
library(shinydashboard)
library(shiny)
library(ggplot2)

ui <-
  dashboardPage(

    dashboardHeader(title="Review Response Data"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Input CSV file", tabName="dashboard"),
        menuItem("Input adrs", tabName="adrs"),
        menuItem("Input adtr", tabName="adtr"),
        menuItem("Response Plot", tabName="swimmer")
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName="dashboard",
                fluidPage(
                  headerPanel(title = "Input CSV file"),
                  sidebarLayout(
                    sidebarPanel(
                      fileInput("file","Upload the file")
                    ),
                    mainPanel(
                      tableOutput("rawData")
                    )
                  )
                )
        ),
        
        tabItem(tabName="adrs",
                fluidPage(
                  headerPanel(title = "Input adrs"),
                  sidebarLayout(
                    sidebarPanel(
                      fileInput(
                        inputId = "adrs",
                        label = "Choose SAS datasets"
                      )
                    ),
                    mainPanel(
                      tableOutput("adrs")
                    )
                  )
                )
        ),
        
        tabItem(tabName="adtr",
                fluidPage(
                  headerPanel(title = "Input adtr"),
                  sidebarLayout(
                    sidebarPanel(
                      fileInput(
                        inputId = "adtr",
                        label = "Choose SAS datasets"
                      )
                    ),
                    mainPanel(
                      tableOutput("adtr")
                    )
                  )
                )
        ),
        
        tabItem(
          tabName="swimmer",
          fluidRow(
            inputPanel( selectInput("USUBJID","Select a patient:", c( ))), 
            plotOutput("swPlot", height=200),
            plotOutput("sdPlot", height=200),
            plotOutput("spPlot", height=200),
            plotOutput("snPlot", height=200)
          ))
      )))
server <- function(input, output, session) {
  output$rawData <- renderDataTable({
    file_to_read=input$file
    if(is.null(file_to_read)){
      return()
    }
  })
  output$adrs <- renderDataTable({
    file_to_read=input$adrs
    if(is.null(file_to_read)){
      return()
    }
  })
  
  output$adtr <- renderDataTable({
    file_to_read=input$adtr
    if(is.null(file_to_read)){
      return()
    }
  })
  
  output$swPlot <- renderPlot({
    rawData <- read.csv(input$file$datapath, header=FALSE)
    vr<-paste(rawData[(rawData$V1=='ADRS'),]$V2)
    
    adrs <- read_sas(input$adrs$datapath)
    data.frame(adrs)
    
    adrs <- eval(parse(text= paste0('subset(adrs,' , vr,')' )))
    adrs$y <- adrs$PARAM
    adrs$x <- adrs$ADY/(365.25/12)
    adrs <- adrs[c("x","y","AVALC","USUBJID")]
    
    updateSelectInput(session, "USUBJID", label = "Select a patient", 
                      choices = c(unique(adrs$USUBJID)), select=input$USUBJID)
    
    dataInput1 <- reactive({ 
      adrs[(adrs$USUBJID == input$USUBJID),]
    })
    
    myPlot <- ggplot(NULL, aes( x=x , y=y) ) +
      labs(title = "Response Over Time",
           x = "Time Since the Start of Treatment (months)", y = "Parameter") +
      geom_point(data=dataInput1(), aes( colour=AVALC, shape=AVALC), size=4) +
      scale_shape_manual(values=1:length(unique(adrs$AVALC)))
      
    print(myPlot) })
  
  
  output$sdPlot <- renderPlot({
    rawData <- read.csv(input$file$datapath, header=FALSE)
    vr<-paste(rawData[(rawData$V1=='ADTR'),]$V2)
    
    adtr <- read_sas(input$adtr$datapath)
    data.frame(adtr)
    
    adtr <- eval(parse(text= paste0('subset(adtr,' , vr,')' )))
    adtr$x <- ifelse(adtr$ABLFL=='Y', 0,adtr$ADY/(365.25/12))
    adtr$y <- adtr$AVAL
    adtr <- adtr[c("x","y", "USUBJID")]
    
    updateSelectInput(session, "USUBJID", label = "Select a patient", 
                      choices = c(unique(adtr$USUBJID)), select=input$USUBJID)
    
    dataInput2 <- reactive({ 
      adtr[(adtr$USUBJID == input$USUBJID),]
    })
    
    myPlot <- ggplot(dataInput2(), aes(x = x, y = y)) + 
      labs(title = "Sum of Diameters", 
           x = "Time Since the Start of Treatment (months)", y = "Sum of Diameters (mm)") +
      geom_line(size=1) +
      geom_point( size=2) 
    
    print(myPlot) }) 
  
  output$spPlot <- renderPlot({
    rawData <- read.csv(input$file$datapath, header=FALSE)
    vr<-paste(rawData[(rawData$V1=='ADTR'),]$V2)
    
    adtr <- read_sas(input$adtr$datapath)
    data.frame(adtr)
    
    adtr <- eval(parse(text= paste0('subset(adtr,' , vr,')' )))
    adtr$x <- ifelse(adtr$ABLFL=='Y', 0, adtr$ADY/(365.25/12))
    adtr$y <- ifelse(adtr$ABLFL=='Y', 0, adtr$PCHG)
    adtr <- adtr[c("x","y", "USUBJID")]
    
    updateSelectInput(session, "USUBJID", label = "Select a patient", 
                      choices = c(unique(adtr$USUBJID)), select=input$USUBJID)
    
    dataInput2 <- reactive({ 
      adtr[(adtr$USUBJID == input$USUBJID),]
    })
    
      myPlot <- ggplot(dataInput2(), aes(x = x, y = y)) + 
        labs(title = "Percent Change from Baseline in Sum of Diameters", 
             x = "Time Since the Start of Treatment (months)", y = "Percent Change from Baseline (%)") +
        geom_line(size=1) +
        geom_point( size=2) 
      
      print(myPlot) }) 
  
  output$snPlot <- renderPlot({
    rawData <- read.csv(input$file$datapath, header=FALSE)
    vr<-paste(rawData[(rawData$V1=='ADTR'),]$V2)
    
    adtr <- read_sas(input$adtr$datapath)
    data.frame(adtr)
    
    adtr <- eval(parse(text= paste0('subset(adtr,' , vr,')' )))
    adtr$x <- ifelse(adtr$ABLFL=='Y', 0, adtr$ADY/(365.25/12))
    adtr$y <- ifelse(adtr$ABLFL=='Y', 0, adtr$PCNSD)
    adtr <- adtr[c("x","y", "USUBJID")]
    
    updateSelectInput(session, "USUBJID", label = "Select a patient", 
                      choices = c(unique(adtr$USUBJID)), select=input$USUBJID)
    
    dataInput2 <- reactive({ 
      adtr[(adtr$USUBJID == input$USUBJID),]
    })
    
    myPlot <- ggplot(dataInput2(), aes(x = x, y = y)) + 
      labs(title = "Percent Change from Nadir in Sum of Diameters", 
           x = "Time Since the Start of Treatment (months)", y = "Percent Change from Nadir (%)") +
      geom_line(size=1) +
      geom_point( size=2) 
    
    print(myPlot) }) 
}
shinyApp(ui=ui, server=server)