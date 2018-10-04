#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(purrr)
library(broom)
library(ggplot2)
library(dplyr)
library(arm)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Experiment simulator"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        sliderInput("nrep",
                    "Number of subject per group:",
                    min = 1,
                    max = 100,
                    value = 5),
        hr(),
        p("select the range values for outcome variable (Y)"),

        numericInput("min_val","min", value="0"),
        numericInput("max_val","max", value="10"),
        hr(),
        uiOutput("slider"),
        
         sliderInput("b1",
                     "effect:",
                     min=-100,
                     max=100,
                     value=2),
         sliderInput("sigma",
                     "unexplained variability",
                     min=0,
                     max=50,
                     value=2),
         
         
         hr()
        
         #verbatimTextOutput("lm")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("simulated data",
            
            fluidPage(
              fluidRow(
                br(),
                column(12, div(align="center", actionButton("button","run experiment")))
                ,
                br(),
                hr(),
                column(6, DT::dataTableOutput("tab")),
                column(6,  plotOutput("plot1"))
              ),
              hr(),
              fluidRow(
                column(12, div(align="center", p("DATA ANALYSIS")))
              )
              
              
            )
            
            
          ),
          tabPanel("simulation of N experiment",
          fluidPage(
            fluidRow(
              br(),
              column(4, div(align="center", numericInput("simul", "# of simulation", value=1))),
              column(8, div(align="center", actionButton("button2","Simulate")))
            )
          )
          
        )

       
        
       
      )
   )
))

# Define server logic required to draw a histogram

server <- function(input, output) {
  
  output$slider <- renderUI({
    sliderInput("b0", "Mean of control group", min=input$min_val, max=input$max_val, value=input$min_value)
  })
  
  
  source("2group.r")
   
  fit<-eventReactive(input$button,{twogroup_fun(input$nrep, input$b0,input$b1,input$sigma)})
  
  df<-eventReactive(input$button,{augment(fit())})
  
  sim<-eventReactive(input$button2,{rerun(input$simul, twogroup_fun(input$nrep, input$b0,input$b1,input$sigma) ) })
  
  ###TABELLA DATI SIMULATI#### 
   output$tab<-DT::renderDataTable(
     
     df() %>% 
       dplyr::select(y, group) %>% 
       mutate(y=round(y, 3)), options = list(pageLength = 10,searching = FALSE,paging = TRUE,autoWidth = TRUE))
   
 ####BOXPLOT DATI SIMULATI####  
   output$plot1<-renderPlot(
     df() %>% 
       ggplot(aes(y=y,x=group))+geom_boxplot()+
       geom_jitter(aes(), alpha=0.9, 
                   position=position_jitter(w=0.1,h=0.1)))
   
}

#####risultati della simulazione di più esperimenti#####




# Run the application 
shinyApp(ui = ui, server = server)

