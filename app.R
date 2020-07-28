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
    
    # http://www.open-meta.org/technology/one-observer-for-all-buttons-in-shiny-using-javascriptjquery/
    tags$head(tags$script(
      "$(document).on('click', 'button', function(e) {
        e.stopPropagation()
        if(typeof BUTTON_CLICK_COUNT == 'undefined') {
          BUTTON_CLICK_COUNT = 1; 
        } else {
          BUTTON_CLICK_COUNT ++;
        }
        Shiny.onInputChange('js.button_clicked', 
          e.target.id + '_' + BUTTON_CLICK_COUNT);
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
gifs <- readr::read_csv("data/gifs.csv", 
                        col_types = cols(url = col_character()))$url

sidebar_inputs <- c("player_names", "total_score", "double_out", "start_game", 
                    "best_of_x", "games_count", "start_tournament")

rv <- reactiveValues()
tournament_scores <- 
  reactiveValues(tab = tibble(p1 = character(), 
                              p2 = character(), 
                              score = character()))
source("helpers/game_init.R")
source("helpers/leg.R")
source("helpers/tournament.R")
source("helpers/start_game.R")

tournament_going <- FALSE


server <- function(input, output, session) {
  
  observeEvent(input$browser, {
    browser()
  })

  observeEvent(input$start_game, {
    .player_names <- get_player_names(input$player_names)
    
    start_game(session, .player_names, sidebar_inputs, input, output, rv)
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
                 img(src = sample(gifs, 1)),
                 br(),
                 actionButton("continue_tournament", "Continue"))
        insertUI("#content", "afterEnd", gif)
        
        updateActionButton(session, "start_game", label = "Start Game")
        
        
        if(tournament_going){
          sc <- rv$game$scoreboard()
          pl_names <- rv$game$player_names
          score_aggr <- 
            tibble(p1 = pl_names[1], p2 = pl_names[2]) %>% 
            left_join(sc, by = c("p1" = "winner")) %>% 
            rename(score_p1 = n) %>% 
            left_join(sc, by = c("p2" = "winner")) %>%
            rename(score_p2 = n) %>% 
            unite(score, score_p1, score_p2, sep = " : ")
          tournament_scores$tab <- bind_rows(tournament_scores$tab, score_aggr)
        } else {
          walk(sidebar_inputs, enable)
        }
        
        
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
  
  observeEvent(input$continue_tournament, {
    updateTabItems(session, "main_body", "Tournament")
  })
  
  # tournament ------------------------------------------------------------
  tournament_tab <- eventReactive(input$start_tournament, {
    .player_names <- get_player_names(input$player_names)
    
    walk(sidebar_inputs, enable)
    
    tournament_init(.player_names, input$games_count)
  })
  
  output$tournament_board <- DT::renderDataTable({
    tournament_going <<- TRUE
    crosstab_players(tournament_tab(), tournament_scores$tab)
  })
  
  observeEvent(input$js.button_clicked, {
    uid <- str_split(input$js.button_clicked, "_")[[1]]
    button <- uid[1]
    if(button == "trngame"){
      players <- uid[2:3]
      start_game(session, players, sidebar_inputs, input, output, rv)
    }
  })
}

shinyApp(ui, server)
