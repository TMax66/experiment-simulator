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
   


#####risultati della simulazione di più esperimenti#####
output$simalpha<-renderPlot(
  sim() %>% 
    map_df(tidy) %>% 
    filter(term == "(Intercept)") %>%
    mutate(simulation=seq(1:n())) %>% 
    ggplot(aes(x = estimate, xmin = estimate-std.error, xmax =estimate+std.error, y=simulation))+
    geom_point() + 
    geom_segment( aes(x = estimate-std.error, xend = estimate+std.error, y=simulation, yend=simulation))+
    labs(x="mean of control group")
)

output$simeff<-renderPlot(
  sim() %>% 
    map_df(tidy) %>% 
    filter(term == "groupgroup2") %>%
    mutate(simulation=seq(1:n())) %>% 
    ggplot(aes(x = estimate, xmin = estimate-std.error, xmax =estimate+std.error, y=simulation))+
    geom_point() + 
    geom_segment( aes(x = estimate-std.error, xend = estimate+std.error, y=simulation, yend=simulation))+
    labs(x="effect")
)

}