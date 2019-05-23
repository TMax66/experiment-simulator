library(shiny)
library(purrr)
library(broom)
library(ggplot2)
library(dplyr)
library(arm)
library(DT)
library(gridExtra)
library(cowplot)

ui <- fluidPage(
   
   # Application title
   titlePanel("Two-group experiment simulator for continue outcome variable"),
   
   ######## Sidebar with a slider input for number of bins######
   sidebarLayout(
      sidebarPanel(
        # sliderInput("nrep",
        #             "Number of subject per group:",
        #             min = 1,
        #             max = 100,
        #             value = 10),
        
        numericInput("nrep",
                     "Number of subjects per group",
                     min = 1,
                     max = 1000,
                     value = 10),
        
        hr(),
        
        numericInput("b0", 
                    "Mean of outcome variable (y) in control group", 
                    #min=1, 
                    #max=2000, 
                     value=20),
    
        numericInput("sigma",
                     "Standard deviation of outcome variable",
                     min=0,
                     max=1000,
                     value=10),
     
         numericInput("b1",
                     "effect size:",
                     min=-1000,
                     max=1000,
                     value=0),
        hr(),
        br(),
        plotOutput("Y"),

         
         
         hr(),
        
        div("Concept and R code by:",br(),
            
            p("",
              a(href="https://www.linkedin.com/in/massimo-tranquillo-1301552a/",target="_blank",
                "Massimo Tranquillo"),align="left", style = "font-size: 8pt")
        )
        
        
         
      ),
      
      ##############PANNELLO PRINCIPALE##############
      mainPanel(
        tabsetPanel(
          ####INTRO####
          tabPanel("Introduction",
                   br(),br(),
                   includeHTML("intro.html")
                   
                   ),
          #########run experiment########
          tabPanel("Run experiment",
             fluidPage(
              fluidRow(
                br(),
                column(12, div(align="center", actionButton("button","run experiment"))),
                br(),
                hr(),
                column(4, DT::dataTableOutput("tab")),
                br(),br(),br(),
                column(8,  plotOutput("plot1"))
              ), 
              fluidRow(
                hr(),
                column(12, DT::dataTableOutput("tab2"))
              )
              )),
        
          
        tabPanel("Simulate N Experiments",
                 
                 fluidRow(
                           br(),
                           column(4, div(align="center", 
                                         numericInput("simul", "# of simulation", value=""))),
                           br(),
                           column(8, div(align="center", 
                                         actionButton("button2","Run simulation")))
                         ),
                 br(),
                 br(),
                 fluidRow(
                   column(4,div(align="center", plotOutput("histeff"))),
                           column(4, div(align="center", plotOutput("simeff"))),
                            column(4, div(align="center", plotOutput("pvalue")))
                   
                         ),
                 br(),hr(),
                 fluidRow(
                   column(12,DT::dataTableOutput("simtab"))
                 )
                 
                 
                 )#,
        #tabPanel("Data Analysis")
      )
)
))


              
              
              
              
              
              
              
              
              
            #   fluidRow(
            #     column(12, div(align="center", p("DATA ANALYSIS For SINGLE EXPERIMENT"))),
            #     tabsetPanel(
            #       tabPanel("t-test",
            #         fluidPage(
            #           fluidRow(
            #             tableOutput("ttest")
            #           )
            #         )
            #       ), 
            #       tabPanel("Anova",
            #         fluidPage(
            #           fluidRow(
            #             
            #           )
            #         )
            # 
            #       ), 
            #       tabPanel("Linear model",
            #                fluidPage(
            #                  fluidRow(
            #                    
            #                  )
            #                )
            #                
            #       ), 
            #       tabPanel("Size effect",
            #                fluidPage(
            #                  fluidRow(
            #                    
            #                  )
            #                )
            #                
            #       ), 
            #       tabPanel("Bayes",
            #                fluidPage(
            #                  fluidRow(
            #                    
            #                  )
            #                )
            #                
            #       )
            #       
            #       
            #       
            #       
            #       
            #       
            #       
            #       
            #     )
            #     
            #     
            #     
            #     
            #   )
            #   
            #   
            # )
            # 
            # 
    
        
        
        # tabPanel("Simulate of N experiment",
        #          fluidPage(
        #            fluidRow(
        #              br(),
        #              column(4, div(align="center", numericInput("simul", "# of simulation", value=1),
        #                            hr(),
        #                            p("select min -max for x-scale"),
        #                            numericInput("xmin_val","min", value=""),
        #                            numericInput("xmax_val","max", value=""))),
        #              column(8, div(align="center", actionButton("button2","Run simulation")))
        #            ),
        #            hr(),
        #            br(),
        #            fluidRow(
        #              
        #              column(6, div(align="center", plotOutput("simalpha"))),
        #              column(6, div(align="center", plotOutput("simeff")))
        #              
        #            )
        #          )
        #          
        # )

       
        
       
      
