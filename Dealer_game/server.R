#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
start <- T





waiting_screen <- tagList(
  spin_heart(),
  #spin_cube_grid(),#spin_fading_circles(),#spin_solar(),
  #we can do progress bar as well https://github.com/JohnCoene/waiter
  #https://rdrr.io/cran/waiter/man/spinners.html
  h3("Thanks for waiting during computation...")
) 

#service minimum
shinyServer <- function(input,output,session){#ce qui est dans shiny server est executer a chaque fois qu'on touche un bouton
  
  print("Server Launched")
  #objet reactif
  #c'est une fonction, il faut mettre des parenthese
  #on ne peut pas creer d'objet R simple à partir d'e inout'objet input
  
  
  cards_per_deck<-reactive(input$numeric_nb_rank*input$numeric_nb_colours+input$numeric_nb_extra)
  cards<-reactive(input$numeric_nb_deck*cards_per_deck())
  
  #cela ca pourrait etre des eventreactifs
  n_rank_max_collec=reactive(input$numeric_nb_rank)
  n_colours_max_collec=reactive(input$numeric_nb_colours)
  n_decks_max_collec=reactive(input$numeric_nb_deck)
  nb_extra<-reactive(input$numeric_nb_extra)
  
  collec_colours<- eventReactive(input$go,input$sl_collection_colours)
  collec_rank<- eventReactive(input$go,input$sl_collection_rank)
  collec_deck<- eventReactive(input$go,input$sl_collection_deck)
  method<-eventReactive(input$go,input$rb_computation)
  
  #update slider to round
  observe({
    if(!(is.na(input$numeric_nb_rank))){
      if (input$numeric_nb_rank!=round(input$numeric_nb_rank,0)) {
        updateNumericInput(session, "numeric_nb_rank",value= round(input$numeric_nb_rank,0))
      }
    }
    
    if(!(is.na(input$numeric_nb_colours))){
      if (input$numeric_nb_colours!=round(input$numeric_nb_colours,0)) {
        updateNumericInput(session, "numeric_nb_colours",value= round(input$numeric_nb_colours,0))
      }
    }
    
    if(!(is.na(input$numeric_nb_deck))){
      if (input$numeric_nb_deck!=round(input$numeric_nb_deck,0)) {
        updateNumericInput(session, "numeric_nb_deck",value= round(input$numeric_nb_deck,0))
      }
    }
    
    if(!(is.na(input$numeric_nb_extra))){
      if (input$numeric_nb_extra!=round(input$numeric_nb_extra,0)) {
        updateNumericInput(session, "numeric_nb_extra",value= round(input$numeric_nb_extra,0))
      }
    }
    
    
    
    
  })
      

  
  #text inside input panel and update slider
  observe({
    updateSliderInput(session,
                      "sl_collection_rank",
                      max = n_rank_max_collec())
    updateSliderInput(session,
                      "sl_collection_colours",
                      max = n_colours_max_collec())
    updateSliderInput(session,
                      "sl_collection_deck",
                      max = n_decks_max_collec())
    
    output$nb_cards_per_deck <-renderText(paste("Cards per deck: ",cards_per_deck()))
    output$nb_cards<-renderText(paste("Total number of cards: ",cards()))
  })

  
  
  #waiter
  w <- Waiter$new()


  
 
  
  #computation
  observeEvent(input$go, {
    waiter_show(html=waiting_screen, color="black")
    t1<-Sys.time()
    if(method()=="Exact (Markov chain)"){
      results<-calcul_exact(nb_hauteur=n_rank_max_collec(),
                            nb_couleur = n_colours_max_collec(),
                            n_deck =n_decks_max_collec(),
                            extra=nb_extra(),
                            nb_success_hauteur=collec_rank(),
                            nb_success_paquet=collec_deck(),
                            nb_success_couleur = collec_colours())
      data<-data.frame(results$p,1:length(results$p))
    }
    
    if(method()=="Monte Carlo"){
      n_simu<-eventReactive(input$go,input$rb_simulation)
      res2<-numeric(n_simu())
      res<-sapply(X=res2,FUN=simulation,
                   nb_hauteur=n_rank_max_collec(),nb_couleur = n_colours_max_collec(),
                  n_deck = n_decks_max_collec(),extra=nb_extra(),
                   nb_success_hauteur=collec_rank(),nb_success_paquet=collec_deck(),nb_success_couleur = collec_colours())
      moyenne<-mean(res)
      p<-table(res)/as.numeric(n_simu())
      results<-list(moyenne,p)
      names(results)<-c("moyenne","p")
      data<-data.frame(results$p)[2:1]
      data[,2]<-as.numeric(as.vector(data[,2]))
    }
    
    t2<-Sys.time()
    res<-difftime(t2,t1)
    my_time<-paste(round(as.numeric(res),3),units(res))
    
    


    #output 
    output$Time<-renderText(my_time)
    output$means_number_cards<-renderText(mean(results$moyenne))
    
    
    #le graphique
    
    names(data)<-c("Probability","Cards_drawn")
    max<-max(data$Probability)
    index<-which(data$Probability>max/100)
    lb<-min(index)
    ub<-max(index)
    data<- data[lb:ub,]
    p<-ggplot(data=data, aes(x=Cards_drawn, y=Probability)) +
      geom_bar(stat="identity",fill="firebrick2")+labs(x="Number of cards drawn",y="Probabilities")
    p
    
    output$number_cards_distribution_plot<-renderPlot(p)
    waiter_hide()
    })

  
  
  

  
  
}
