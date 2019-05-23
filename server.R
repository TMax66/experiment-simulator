server <- function(input, output) {
  
  # output$slider <- renderUI({
  #   sliderInput("b0", "Mean of control group", min=input$min_val, max=input$max_val, value=20)
  # })
  
  
  source("2group.r")
   
  fit<-eventReactive(input$button,{twogroup_fun(input$nrep, input$b0,input$b1,input$sigma)})
  
  df<-eventReactive(input$button,{augment(fit())})
  
  sim<-eventReactive(input$button2,{rerun(input$simul, twogroup_fun(input$nrep, input$b0,input$b1,input$sigma) ) })
  
  ###TABELLA DATI SIMULATI#### 
   output$tab<-DT::renderDataTable(server = FALSE,
     
     df() %>% 
       mutate(factor(group)) %>% 
       mutate(group=recode(group, group1="Control",group2="Treatment")) %>% 
       dplyr::select(y, group) %>% 
       mutate(y=round(y, 3)),colnames = c('Subject' = 1),
     caption = 'Simulated data',
     class = 'cell-border stripe', extensions = 'Buttons', options = list(dom="Brtip",
         pageLength = 10,searching = FALSE,paging = TRUE,autoWidth = TRUE,
                                             buttons = c("csv",'excel')))
   
 ####BOXPLOT DATI SIMULATI####  
   output$plot1<-renderPlot(
     plot_grid( (df() %>% 
                   ggplot(aes(x=group, y=y))+geom_boxplot(fill="firebrick4")+labs(x="")+
                   labs(title="sample distribution of y")+
                   scale_x_discrete(labels = c("Control","Treatment"))+
                   geom_jitter(aes(), alpha=0.9, 
                               position=position_jitter(w=0.1,h=0.1))+coord_flip()),
                (
                  
                  fit()%>% 
                    tidy(conf.int = TRUE) %>%
                    #filter(term=="group") %>% 
                    ggplot(aes(term, estimate))+
                    geom_point()+labs(x="", y="")+
                    geom_pointrange(aes(ymin = conf.low, ymax = conf.high))+
                    labs(title="95% C.I. estimate of effect size and control group mean")+
                    scale_x_discrete(labels = c("control group",'effect size'))+
                    coord_flip()),
               ncol=1,align='v')
   )
     
#####tabella summary per gruppo####
     output$tab2<-DT::renderDataTable(server = FALSE,
            df() %>% 
              mutate(factor(group)) %>% 
              mutate(group=recode(group, group1="Control",group2="Treatment")) %>% 
            group_by(group) %>% 
            summarise("N"=n(),
                      "mean"=mean(y),
                      "sd"=sd(y),
                      "median"=median(y),
                      "25%"=quantile(y, 0.25),
                      "50%"=quantile(y, 0.50),
                      "75%"=quantile(y, 0.75),
                      "min"=min(y),
                      "max"=max(y)) %>% 
              mutate_if(is.numeric, funs(round(.,digits = 2))),
            caption = 'Descriptive statistics of sample by group',
            class = 'cell-border stripe', rownames=FALSE, option=list(dom = 't',searching = FALSE,paging = TRUE,autoWidth = TRUE)
            )
     
     
     
     
     
     # df() %>% 
     #   ggplot(aes(y=y,x=group))+geom_boxplot()+
     #   geom_jitter(aes(), alpha=0.9, 
     #               position=position_jitter(w=0.1,h=0.1))+
     #   scale_y_continuous(limits = c(input$min_val,input$max_val))
 
  
   
  output$Y<-renderPlot({   
    mean=input$b0
    sd=input$sigma
    each=1e4
    
    if (input$b1==0)
    {
    dat<-tibble(y = c(rnorm(each, mean=mean, sd=sd)))
    dat %>% 
      ggplot(aes(x=y)) + geom_density(alpha=.08, adjust=2.5)
    }
    else
    {   
    dat <- tibble(cond = factor(rep(c("Control","Treatment"), each=each)), 
                  y = c(rnorm(each, mean=mean, sd=sd),
                        rnorm(each, mean=mean+input$b1, sd=sd)))
    dat %>% 
      ggplot(aes(x=y, fill=cond)) + geom_density(alpha=.3, adjust=2.5, )+
      scale_fill_viridis_d()+theme(legend.title = element_blank())
    }
  }
  )
  
  


output$simeff<-renderPlot(
  sim() %>% 
    map_df(tidy) %>% 
    filter(term == "groupgroup2") %>%
    mutate(simulation=seq(1:n())) %>% 
    ggplot(aes(x = estimate, xmin = estimate-std.error, xmax =estimate+std.error, y=simulation))+
    geom_point() + 
    geom_segment( aes(x = estimate-std.error, xend = estimate+std.error, y=simulation, yend=simulation))+
    #scale_x_continuous(limits=c(input$xmin_val,input$xmax_val))+
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

output$simtab<-DT::renderDataTable(server = FALSE,
  sim() %>%
    map_df(tidy) %>% 
    filter(term == "groupgroup2") %>% 
    dplyr::select(estimate,std.error,statistic,"p value"=p.value) %>% 
    mutate_at('p value', round, 5) %>% 
    mutate_at(1:3, round, 3) %>% 
    datatable(colnames = c('simulation' = 1),class = 'cell-border stripe',extensions = 'Buttons',
              options=list(dom="Brtp",pageLength = 10,searching = FALSE,paging = TRUE,autoWidth = TRUE,
                            buttons = c("csv",'excel')))
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