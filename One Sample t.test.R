library(readxl)
library(shiny)
ui = fluidPage(
  titlePanel("one-sample t-test"),
  tabsetPanel(
    tabPanel("Vector", fluid = TRUE, 
             sidebarLayout(
               sidebarPanel( textInput('vec','Enter a vector of data' , 'e.g., 0,1,2'),
                             numericInput('a1',' Mean(mu0):',0) ,
                             selectInput("alt1", "Alternative hypothesis:",c("two.sided" = "two.sided",
                                                                             "greater" = "greater",
                                                                             "less" = "less")),
                             sliderInput('conf1','Confidence level:',min =0.01 ,max=0.99,value=0.95)),
               mainPanel(fluidRow(splitLayout(style = "border: 1px solid silver;",
                                              cellArgs = list(style = "padding: 1px"),cellWidths = c("50%", "50%"),
                                              verbatimTextOutput('norm1'), 
                                              plotOutput('plot1'))),
                         fluidRow(splitLayout(style = "border: 1px solid silver;",
                                              cellArgs = list(style = "padding: 1px"),cellWidths = c("50%", "50%"),
                                              verbatimTextOutput('b'),
                                              plotOutput('plott1')))
               )
             )
    ),
    tabPanel("Excel", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(fileInput('file1', 'Choose xlsx file', accept = c(".xlsx","xls" )),
                            numericInput('col','Column number:',1),
                            numericInput('sheet','Sheet number:',1),
                            
                            numericInput('a2','Mean(mu0):',0),
                            selectInput("alt2", "Alternative hypothesis:",c("two.sided" = "two.sided",
                                                                            "greater" = "greater",
                                                                            "less" = "less")),
                            sliderInput('conf2','Confidence level:',min =0.01 ,max=0.99,value=0.95) 
               ),
               mainPanel(fluidRow(splitLayout(style = "border: 1px solid silver;",
                                              cellArgs = list(style = "padding: 1px"),cellWidths = c("50%", "50%"),
                                              verbatimTextOutput("norm2"),
                                              plotOutput('plot2'))),
                         fluidRow(splitLayout(style = "border: 1px solid silver;",
                                              cellArgs = list(style = "padding: 1px"),cellWidths = c("50%", "50%"), 
                                              verbatimTextOutput("e"),
                                              plotOutput('plott2')))
               )
             )
    )
  )
)



server <- function(input, output) {
  output$norm1 <- renderPrint({
    x <- as.numeric(unlist(strsplit(input$vec,",")))
    print(shapiro.test(x))
    if(shapiro.test(x)$p.value> 0.05){print('p.value is more than 0.05.')
      print('The data follow a normal distribution so can use t.test')}
    else{ if(length(x)>30){print('p.value is less than 0.05 but sample size is larger than 30.')
      print(' You can use t.test.')}
      else{print('p.value is less than 0.05 and sample size is not larger than 30.')
        print(' You can not use t.test.')} }  
    
  })
  output$plot1 <- renderPlot({
    x <- as.numeric(unlist(strsplit(input$vec,",")))
    qqnorm(x)
    qqline(x,,col='red')},height = 300, width = 400)
  
  output$b <- renderPrint({ 
    x <- as.numeric(unlist(strsplit(input$vec,",")))
    print(t.test(x,mu= input$a1 , alt=input$alt1 , conf.level=input$conf1))
    t1 <- t.test(x,mu= input$a1 , alt=input$alt1 , conf.level=input$conf1)
    if(t1$p.value > 0.05){print('p-value is more than 0.05.')
      print('We do not reject the null hypothesis that there is no difference between the means.')}
    else {print('p-value is less than 0.05.')
      print('We reject the null hypothesis that there is no difference between the means.')}
  })
  output$plott1 <- renderPlot({x <- as.numeric(unlist(strsplit(input$vec,",")))
  boxplot(x,col=12)},height = 300, width = 400)
  
  
  output$norm2 <- renderPrint({
    f <- input$file1 
    data <- read_excel(f$datapath,sheet=input$sheet,col_names =T)
    print(shapiro.test(data[[input$col]]))
    if(shapiro.test(data[[input$col]])$p.value> 0.05){print('p.value is more than 0.05.')
      print('The data follow a normal distribution so can use t.test')}
    else{ if(length(data[[input$col]])>30){print('p.value is less than 0.05 but sample size is larger than 30.')
      print(' You can use t.test.')}
      else{print('p.value is less than 0.05 and sample size is not larger than 30.')
        print(' You can not use t.test.')} }  
  })
  output$plot2 <- renderPlot({
    f <- input$file1
    data <- read_excel(f$datapath,sheet=input$sheet,col_names =T)
    qqnorm(data[[input$col]])
    qqline(data[[input$col]],col='red')},height = 300, width = 400)
  output$e <- renderPrint({
    f <- input$file1 
    data <- read_excel(f$datapath,sheet=input$sheet,col_names = T)
    t2 <- t.test(data[[input$col]],mu= input$a2 , alt=input$alt2 , conf.level=input$conf2 )
    print(t2)
    if(t2$p.value> 0.05){print('p-value is more than 0.05.')
      print('We do not reject the null hypothesis that there is no difference between the means.')}
    else {print('p-value is less than 0.05.')
      print('We reject the null hypothesis that there is no difference between the means.')}
  })
  output$plott2 <- renderPlot({
    f <- input$file1
    data <- read_excel(f$datapath,sheet=input$sheet,col_names = T)
    boxplot(data[[input$col]],col=12)
  },height = 300, width = 400) }
shinyApp(ui,server)



