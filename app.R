library(shiny)
library(R6)
library(attempt)
library(tidyverse)
library(shinyjs)
library(shinydashboard)
library(glue)
library(shinyWidgets)
library(DT)

sidebar <- 
  dashboardSidebar(
    useShinyjs(),
    tags$script("$('#browser').hide();"),
    
    textInput("player_names", "Enter the names of the players (comma seperated)", 
              value = "Anna, Bert, Charlie"), 
    radioButtons('total_score', "Goal:", choices = c("301", "501"), 
                 inline = TRUE), 
    radioButtons('double_out', "Finish:", 
                 choices = c("Sinlge Out", "Double Out"), 
                 inline = TRUE), 
    actionButton('start_game', "Start Leg"), 
    actionButton("browser", "browser")
    
)
    
body <- 
  dashboardBody(
    useShinyjs(),
    
    tags$head(
      tags$script(src = "enter_button.js"), 
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
    ),
    tags$style(HTML("
        input[type=number] {
              -moz-appearance:textfield;
        }
        input[type=number]::{
              -moz-appearance:textfield;
        }
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
              -webkit-appearance: none;
              margin: 0;
        }
    ")),
    
    fluidPage(
      div(id = "content")
    )
  )

ui <- 
  dashboardPage(
    dashboardHeader(), 
    sidebar, 
    body,
  )


rv <- reactiveValues()
source("helpers/game_init.R")
source("helpers/leg.R")

server <- function(input, output, session){
  observeEvent(input$browser,{
    browser()
  })
  
  observeEvent(input$start_game, {
    # update sidebar
    
    .player_names <- 
      str_split(input$player_names, ",", simplify = FALSE)[[1]] %>% 
      str_trim()
    
    if(length(.player_names) > 3){
      sendSweetAlert(session, "Error", "More than 3 players are not supported", 
                     type = "error")
      return(NULL)
    }
   
    c('player_names', 'total_score', 'double_out', 'start_game') %>% 
      walk(disable)
    
    updateActionButton(session, 'start_game', label = "Game started!")
    
    
    rv$game <- LegNPlayers$new(
      player_names = .player_names, 
      total_score  = input$total_score %>% as.numeric(), 
      double_out   = input$double_out == "Double Out")
  
    
    output$next_player <- renderText(rv$game$players[rv$game$next_score])
    
    insertUI(
      '#content', 'beforeBegin', 
      tagList(
        div(id = "score_input", 
            textOutput('next_player'), 
            numericInput('score_count', "Score", NA_real_, 
                         min = 0, max = 180, step = 1)),
        actionButton('score', "Score") %>% hidden()
        ))
  })
  
  observeEvent(rv$game$players, {
    map(rv$game$legs, iBox, output = output, .width = 12/length(rv$game$players))
  })
  
  # observe({
  #   isolate(req(rv$game$legs))
  #   table_id <- glue('table_{rv$game$legs[[1]]$player_name}')
  #   output[[table_id]] <- DT::renderDataTable(rv$game$legs[[1]]$hist)
  # })

  observeEvent(input$score, {
    req(input$score_count)
    rv$game$score(input$score_count)
    next_player <- rv$game$players[rv$game$next_score]
    output$next_player <- renderText(next_player)

    if(exists("class_to_add")){
      removeClass(class = class_to_add, selector = '#score_count')
    }
    
    class_to_add <<- glue('dart-player{rv$game$next_score}')
    addClass(class = class_to_add, selector = '#score_count')
    
    updateNumericInput(session, 'score_count', value = NA_real_)
  })
}

shinyApp(ui, server)