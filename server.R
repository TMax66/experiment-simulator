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
# output$simalpha<-renderPlot(
#   sim() %>% 
#     map_df(tidy) %>% 
#     filter(term == "(Intercept)") %>%
#     mutate(simulation=seq(1:n())) %>% 
#     ggplot(aes(x = estimate, xmin = estimate-std.error, xmax =estimate+std.error, y=simulation))+
#     geom_point() + 
#     geom_segment( aes(x = estimate-std.error, xend = estimate+std.error, y=simulation, yend=simulation))+
#     labs(x="mean of control group")
# )

output$simeff<-renderPlot(
  sim() %>% 
    map_df(tidy) %>% 
    filter(term == "groupgroup2") %>%
    mutate(simulation=seq(1:n())) %>% 
    ggplot(aes(x = estimate, xmin = estimate-std.error, xmax =estimate+std.error, y=simulation))+
    geom_point() + 
    geom_segment( aes(x = estimate-std.error, xend = estimate+std.error, y=simulation, yend=simulation))+
    scale_x_continuous(limits=c(input$xmin_val,input$xmax_val))+
    labs(x="effect")
)

  
output$histeff<-renderPlot(
  sim() %>%
    map_df(tidy) %>% 
    filter(term == "groupgroup2") %>%
    ggplot( aes(estimate) ) +
    geom_density(fill = "blue", alpha = .5) +
    geom_vline( xintercept = input$b1)+
    labs(x="effect")

  
)


output$pvalue<-renderPlot(
  sim() %>%
    map_df(tidy) %>%
    filter(term == "groupgroup2") %>%
    ggplot( aes(p.value) ) +
    geom_histogram(color="black",fill = "blue", alpha = .5)+
    geom_vline(xintercept = 0.05, lty=3)
)

# output$sigma<-renderPlot(
#   sim() %>%
#     map_dbl(~summary(.x)$sigma) %>%
#     data.frame(sigma = .) %>%
#     ggplot( aes(sigma) ) +
#     geom_density(fill = "blue", alpha = .5) +
#     geom_vline(xintercept = input$sigma)
# )

output$simtab<-DT::renderDataTable(
  sim() %>%
    map_df(tidy) %>% 
    filter(term == "groupgroup2") %>% 
    dplyr::select(estimate,std.error,statistic,"p value"=p.value) %>% 
    mutate_at('p value', round, 5) %>% 
    mutate_at(1:3, round, 3) %>% 
    datatable(colnames = c('simulation' = 1),
              options=list(pageLength = 10,searching = FALSE,paging = TRUE,autoWidth = TRUE))
)



####t-test####

output$ttest<-renderTable(
  df() %>% 
    dplyr::select(y, group) %>% 
    mutate(y=round(y, 3)) %>% 
    do(tidy(t.test(y~group, data=.))) %>% 
    mutate("difference"=estimate2-estimate1) %>% 
    dplyr::select(difference)
  
)





####Anova####



####Linear model####


###Bayes####



}