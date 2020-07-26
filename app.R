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
    
    textInput("player_names", 
              "Enter the names of the players (comma seperated)", 
              value = "Anna, Bert, Charlie"), 
    radioButtons("total_score", "Goal:", choices = c("301", "501"), 
                 inline = TRUE), 
    radioButtons("double_out", "Finish:", 
                 choices = c("Sinlge Out", "Double Out"), 
                 inline = TRUE), 
    radioButtons("best_of_x", "Best of:", 
                 choices = seq(1, 9, by = 2), selected = 7, 
                 inline = TRUE), 
    radioButtons("games_count", "Number of games per pair of players:", 
                 choices = 1:2, selected = 1, inline = TRUE),
    fluidRow(
      id = "buttons-sidebar",
      style = "margin-left: 0px",
      actionButton("start_game", "Start Leg"), 
      actionButton("start_tournament", "Start Tournament")
    ),
    
    actionButton("browser", "browser")
    
)
    
body <- 
  dashboardBody(
    useShinyjs(),
    
    tags$head(
      tags$script(src = "enter_button.js"), 
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
    ),
    # hide + - buttons in numeric input
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
      # placeholders for future content
      div(id = "starting-player"),
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

server <- function(input, output, session) {
  
  observeEvent(input$browser, {
    browser()
  })

  observeEvent(input$start_game, {
    .player_names <- 
      str_split(input$player_names, ",", simplify = FALSE)[[1]] %>% 
      str_trim()
    
    if (length(.player_names) > 3) {
      sendSweetAlert(session, "Error", "More than 3 players are not supported", 
                     type = "error")
      return(NULL)
    }
   
    c("player_names", "total_score", "double_out", "start_game", 
      "best_of_x", "games_count", "start_tournament") %>% 
      walk(disable)
    
    updateActionButton(session, "start_game", label = "Game started!")
    
     walk(.player_names, ~{
      .id_player <- glue("player_starts_{.x}")
      .ui <- actionButton(glue("player_starts_{.x}"), .x)
      insertUI("#starting-player", "beforeEnd", .ui)
      
     observeEvent(input[[.id_player]], {

        rv$game <- game_n_players$new(
          player_names = .player_names,
          total_score  = input$total_score %>% as.numeric(),
          double_out   = input$double_out == "Double Out", 
          best_of = input$best_of_x %>% as.numeric(), 
          player_nr_to_start_first_leg = which(.player_names == .x))
        
        rv$leg <- rv$game$leg
        
        output$leg_nr      <- renderText(
          as.character(rv$game$get_leg_id()))
        
        output$next_player <- renderText(
          rv$leg$players[rv$leg$next_score])
        
        output$scoreboard  <- DT::renderDataTable(
          rv$game$scoreboard_dt())
        
        removeUI("#starting-player")
        
        insertUI(
          "#content", "beforeBegin",
          tagList(
            div(id = "score_input",
                textOutput("leg_nr"), 
                textOutput("next_player"),
                numericInput("score_count", "Score", NA_real_,
                             min = 0, max = 180, step = 1), 
                DT::dataTableOutput("scoreboard", width = "200px")),
            actionButton("score", "Score") %>% hidden(),
            actionButton("score_rest", "Score Rest") %>% hidden()
          ), immediate = TRUE)
        
        class_to_add <- glue("dart-player{rv$leg$next_score}")
        addClass(class = class_to_add, selector = "#score_count")
      })
    })
  })
  
  observeEvent(rv$leg$players, {
    walk(rv$leg$legs, i_box, input = input, output = output, 
         .width = 12 / length(rv$leg$players))
  })
 
  # Function that is called whenever the scoring is triggered by either
  # pressing "Enter" or "r"
  scoring_event <- function(value, rest = FALSE) {
    scoring_fun <- if (rest) rv$leg$score_rest else rv$leg$score
    
    scoring_fun(value)
    
    if (!is.na(rv$leg$has_finished)) {
      rv$has_finished <- rv$leg$has_finished
    }
    
    next_player <- rv$leg$players[rv$leg$next_score]
    output$next_player <- renderText(next_player)
    
    if (exists("class_to_add")) {
      removeClass(class = class_to_add, selector = "#score_count")
    }
    
    class_to_add <<- glue("dart-player{rv$leg$next_score}")
    addClass(class = class_to_add, selector = "#score_count")
    
    updateNumericInput(session, "score_count", value = NA_real_)
  }
  
  # Event triggered by pressing "Enter"
  observeEvent(input$score, {
    req(input$score_count)
    scoring_event(input$score_count, FALSE)
  })
  
  # Event triggered by pressing "r"
  observeEvent(input$score_rest, {
    req(input$score_count)
    scoring_event(input$score_count, TRUE)
  })
  
  # Event triggered when a leg is finished
  observeEvent(rv$has_finished, {
    if (!is.na(rv$has_finished)) {
      print(rv$leg$has_finished)
      removeUI("#content >div", multiple = TRUE)
      rv$game$is_won()
      rv$game <- rv$game$player_won(rv$has_finished)
      
      
      if (rv$game$is_won()) {
        # show win message when the game is over and enable the user
        # to start the next game
        removeUI("#score_input")
      } else {
        # leg finished and start new leg
        rv$leg <- rv$game$leg
        
        # update next player to score
        next_player <- rv$leg$players[rv$leg$next_score]
        output$next_player <- renderText(next_player)
        
        class_to_add <<- glue("dart-player{rv$leg$next_score}")
        addClass(class = class_to_add, selector = "#score_count")
        
        rv$has_finished <- NA_character_
        output$leg_nr <- renderText(as.character(rv$game$get_leg_id()))
        output$scoreboard <- DT::renderDataTable(rv$game$scoreboard_dt())
      }
      
    }
  }, ignoreInit = TRUE)

}

shinyApp(ui, server)
