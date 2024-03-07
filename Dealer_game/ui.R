#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
shinyUI(
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", 
                type = "text/css", 
                href = "style.css")
    ), 



    
    
    fluidRow(
      column(6, h1("Dealer Game")),
      column(3, img(src = "logo.png", height = 80)),
      column(3, img(src = "cards.png", height = 80))
    ),
    titlePanel("Parameters"),
    sidebarLayout(
      sidebarPanel(
        
        h5("Parameters of the decks"),
        wellPanel(
                  numericInput("numeric_nb_rank",
                                label="Number of Ranks",
                                value=13,
                                min=1, max=n_rank_max, 
                                step=1),
                   numericInput("numeric_nb_colours",
                                 label="Number of coulours",
                                 value=4,
                                 min=1, max=n_colours_max, 
                                 step=1),
                    numericInput("numeric_nb_extra",
                                  label="Number of Jokers",
                                  value=2,
                                  min=0, max=10, 
                                  step=1),
                   fluidRow(
                     #h5("Number of cards per deck:"),
                     textOutput("nb_cards_per_deck")
                   ),
        
                   numericInput("numeric_nb_deck",
                                             label="Number of Decks",
                                             value=10,
                                             min=1, max=n_decks_max, 
                                             step=1),
                   textOutput("nb_cards")),
        h5("Parameters on the collection"),
        
        #collection
        ###############################################
        wellPanel(
          sliderInput("sl_collection_rank",
                              label="Number of ranks to collect",
                              min=0,
                              max=n_rank_max,
                              value=13),
                  sliderInput("sl_collection_colours",
                              label="Number of colours to collect",
                              min=0,
                              max=n_colours_max,
                              value=4),
                  sliderInput("sl_collection_deck",
                              label="Number of decks to collect",
                              min=1,
                              max=n_decks_max,
                              value=10)),
        
        
        
        ######################################
        h5("Computation's parameters"),
        wellPanel(radioButtons("rb_computation",
                               label="Choose your the computation method:",
                               choices=c("Exact (Markov chain)","Monte Carlo"),
                               selected="Exact (Markov chain)"),
                  conditionalPanel(
                    condition = "input.rb_computation == 'Monte Carlo'",
                    radioButtons("rb_simulation", "How many simulations:",
                                choices=c(100,1000,10000,100000),selected=1000
                    )
                  )),
         
        

      actionButton("go", "Shuffle up and Deal"),
      width = 4,
      height=2),#end side panels
      

     
      mainPanel(
        
        
        h2("Results"),
        useWaiter(),
          #h3("Number of card to draw to complete the collection"),#html niveau de titre
        fluidRow(
          column(3, h3("Execution time:")),
          column(6, h3(textOutput("Time"))),
        ),
          
          
          #withSpinner(textOutput("Time")),
          br(),
        
        fluidRow(
          column(8,  h3("Means number of cards to draw to complete the collection:")),
          column(6, h3(textOutput("means_number_cards"))),
        ),
          #withSpinner(textOutput("means_number_cards")),
          br(),
          h3("Distribution of the number of cards to draw to complete the collection:"),
          plotOutput("number_cards_distribution_plot",width = "900", height = "700px")
          #withSpinner(plotOutput("number_cards_distribution_plot")) 
#end with spinner
      )
    )

  )#end fluid page
)
