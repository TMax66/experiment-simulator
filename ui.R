library(shiny)
library(purrr)
library(broom)
library(ggplot2)
library(dplyr)
library(arm)
library(DT)
ui <- fluidPage(
   
   # Application title
   titlePanel("Experiment simulator"),
   
   ######## Sidebar with a slider input for number of bins######
   sidebarLayout(
      sidebarPanel(
        sliderInput("nrep",
                    "Number of subject per group:",
                    min = 1,
                    max = 100,
                    value = 5),
        hr(),
        p("select the range values for outcome variable (Y)"),

        numericInput("min_val","min", value="10"),
        numericInput("max_val","max", value="50"),
        hr(),
        uiOutput("slider"),
        
         sliderInput("b1",
                     "effect:",
                     min=-50,
                     max=50,
                     value=0),
         sliderInput("sigma",
                     "unexplained variability",
                     min=0,
                     max=50,
                     value=2),
         
         
         hr()
        
         
      ),
      
      ##############PANNELLO PRINCIPALE##############
      mainPanel(
        tabsetPanel(
          #########run experiment########
          tabPanel("Run experiment",
             fluidPage(
              fluidRow(
                br(),
                column(12, div(align="center", actionButton("button","run experiment"))),
                br(),
                hr(),
                column(6, DT::dataTableOutput("tab")),
                br(),br(),br(),
                column(6,  plotOutput("plot1"))
              )
              )),
        
          
        tabPanel("Simulate N Experiments",
                 
                 fluidRow(
                           br(),
                           column(4, div(align="center", 
                                         numericInput("simul", "# of simulation", value=1))),
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
                 
                 
                 ),
        tabPanel("Data Analysis")
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

       
        
       
      
