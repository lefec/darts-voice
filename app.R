library(shiny)
library(R6)
library(attempt)
library(tidyverse)
library(shinyjs)
library(shinydashboard)
library(glue)
library(shinyWidgets)
library(DT)


# ui ----------------------------------------------------------------------
sidebar <- 
  dashboardSidebar(
    useShinyjs(),
    
    # focus on the score_count after starting the game. So you don't have to
    # manually select the score input by mouse.
    # https://stackoverflow.com/q/38362861
    tags$head(tags$script(
      "Shiny.addCustomMessageHandler('focus-score',
        function(NULL) {
          document.getElementById('score_count').focus();
       });"
    )),
    
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
                 choices = seq(1, 9, by = 2), selected = 1, 
                 inline = TRUE), 
    radioButtons("games_count", "Number of games per pair of players:", 
                 choices = 1:2, selected = 1, inline = TRUE),
    fluidRow(
      id = "buttons-sidebar",
      style = "margin-left: 0px",
      actionButton("start_game", "Start Game"), 
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
    
    # fluidPage(
      tabBox(id = "main_body", width = 12,
             
        tabPanel(
         title =  "Tournament", 
          DT::dataTableOutput("tournament_board")
        ), 
        
        tabPanel(
          title = "Game", 
          # anchors for content
          div(id = "starting-player"),
          div(id = "content")
        ), 
        
        tabPanel(
          title = "Stats")
      )
    # )
  )

ui <- 
  dashboardPage(
    dashboardHeader(), 
    sidebar, 
    body,
  )


# server ------------------------------------------------------------------

rv <- reactiveValues()
source("helpers/game_init.R")
source("helpers/leg.R")
source("helpers/tournament.R")
gifs <- readr::read_csv("data/gifs.csv", 
                        col_types = cols(url = col_character()))$url

sidebar_inputs <- c("player_names", "total_score", "double_out", "start_game", 
                    "best_of_x", "games_count", "start_tournament")

server <- function(input, output, session) {
  
  observeEvent(input$browser, {
    browser()
  })

  observeEvent(input$start_game, {
    .player_names <- get_player_names(input$player_names)
    
    if (length(.player_names) > 3) {
      sendSweetAlert(session, "Error", "More than 3 players are not supported", 
                     type = "error")
      return(NULL)
    }
    
    removeUI("#game-winner-animation")
   
    walk(sidebar_inputs, disable)
    
    updateActionButton(session, "start_game", label = "Game started!")
    
    start_observers <<- list()

    updateTabItems(session, "main_body", "Game")
    
    walk(.player_names, ~{
      .id_player <- glue("player_starts_{.x}")
      .ui <- actionButton(glue("player_starts_{.x}"), .x)
      insertUI("#starting-player", "beforeEnd", .ui)
      
      obs <- observeEvent(input[[.id_player]], {
        rv$game <- game_n_players$new(
          player_names = .player_names,
          total_score  = input$total_score %>% as.numeric(),
          double_out   = input$double_out == "Double Out", 
          best_of = input$best_of_x %>% as.numeric(), 
          player_nr_to_start_first_leg = which(.player_names == .x))
        
        rv$leg <- rv$game$leg
        
        output$leg_nr <- renderText(
          as.character(rv$game$get_leg_id()))
        
        output$next_player <- renderText(
          rv$leg$players[rv$leg$next_score])
        
        output$scoreboard <- DT::renderDataTable(
          rv$game$scoreboard_dt())
        
        removeUI("#starting-player > button", multiple = TRUE)
        
        .ui <- tagList(
          div(id = "score_input",
              textOutput("leg_nr"), 
              textOutput("next_player"),
              numericInput("score_count", "Score", NA_real_,
                           min = 0, max = 180, step = 1), 
              DT::dataTableOutput("scoreboard", width = "400px")),
          actionButton("score", "Score") %>% hidden(),
          actionButton("score_rest", "Score Rest") %>% hidden()
        )
        
        insertUI("#content", "beforeBegin", .ui, immediate = TRUE)
        
        class_to_add <- glue("dart-player{rv$leg$next_score}")
        addClass(class = class_to_add, selector = "#score_count")
        
        # trigger js function to focus the curson on score_count
        session$sendCustomMessage(type = "focus-score", message = list(NULL))
      }, ignoreInit = TRUE)
      
      start_observers <<- c(start_observers, obs)
    })
    
  })
  
  observeEvent(rv$leg$players, {
    walk(rv$leg$legs, i_box, input = input, output = output, 
         .width = 12 / length(rv$leg$players))
  })
 
  # Function that is called whenever the scoring is triggered by either
  # pressing "Enter" or "r"
  scoring_event <- function(value, rest = FALSE) {
    
    valid_score <- rv$leg$score(value, rest)
    
    
    if (valid_score) {
      if (!is.na(rv$leg$has_finished)) {
        rv$has_finished <- rv$leg$has_finished
      }
      
      next_player <- rv$leg$players[rv$leg$next_score]
      output$next_player <- renderText(next_player)
      
      # change the color of the score box to fit the player color
      if (exists("class_to_add")) {
        removeClass(class = class_to_add, selector = "#score_count")
      }
      
      class_to_add <<- glue("dart-player{rv$leg$next_score}")
      addClass(class = class_to_add, selector = "#score_count")
      
      # reset the numeric input to be ready to fill in the next score
    } else {
      sendSweetAlert(session, "Error", glue("{value} is not a valid score"), 
                     type = "error")
    }
    # reset score input
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
  
  # Event triggered when a leg (or game) is finished
  observeEvent(rv$has_finished, {
    if (!is.na(rv$has_finished)) {
      print(rv$leg$has_finished)
      
      removeUI("#content >div", multiple = TRUE)
      rv$game <- rv$game$player_won(rv$has_finished)
      
      
      if (rv$game$is_won()) {
        # show win message when the game is over and enable the user
        # to start the next game
        removeUI("#score_input")
        removeUI("#score")
        removeUI("#score_rest")
        
        # destroy all observeEvents that start the game because they will
        # be rebuild for the next game. This allows to change the players 
        # between games.
        walk(start_observers, ~.x$destroy())

        gif <- p(id = "game-winner-animation", 
                 glue("Winner: {rv$has_finished}"), 
                 br(),
                 img(src = sample(gifs, 1)))
        insertUI("#content", "afterEnd", gif)
        
        updateActionButton(session, "start_game", label = "Start Game")
        walk(sidebar_inputs, enable)
        
        
      } else {
        # leg finished and start new leg
        rv$leg <- rv$game$leg
        
        # update next player to score
        next_player <- rv$leg$players[rv$leg$next_score]
        output$next_player <- renderText(next_player)
        
        class_to_add <<- glue("dart-player{rv$leg$next_score}")
        addClass(class = class_to_add, selector = "#score_count")
        
        # reset the winning player
        rv$has_finished <- NA_character_
        output$leg_nr <- renderText(as.character(rv$game$get_leg_id()))
        output$scoreboard <- DT::renderDataTable(rv$game$scoreboard_dt())
      }
      
    }
  }, ignoreInit = TRUE)
  

  # tournament ------------------------------------------------------------
  observeEvent(input$start_tournament, {
    .player_names <- get_player_names(input$player_names)

    output$tournament_board <- DT::renderDataTable(
      crosstab_players(.player_names, input$games_count)
    )
  })

}

shinyApp(ui, server)
