player_selection_ui <- function(id, player_names) {
  ns <- NS(id)
  .choices <- as.list(seq_along(player_names))
  names(.choices) <- player_names

  tagList(
    radioButtons(ns("player"), label = "Player to start", choices = .choices),
    actionButton(ns("start"), label = "Start")
  )
}

player_selection_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    eventReactive(input$start, {
      input$player
    })
  })
}



match_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("MATCH"),
    div(id = "player-start-selection"),
    h1("PLAYER"),
    textOutput(ns("player"))
  )
}


#' match_settings is a names list of player_names, score, double_out
match_server <- function(id, match_settings) {
  moduleServer(id, function(input, output, session) {

    player_to_start <-
      if (is.null(match_settings$player_nr_to_start_first_leg)) {
        ui <- player_selection_ui(id = session$ns("sel"),
                                  player_names = match_settings$player_names)
        insertUI("#player-start-selection", ui = ui)
        player_selection_server("sel")
      } else {
        reactive(match_settings$player_nr_to_start_first_leg)
      }

    observeEvent(player_to_start(), {
      if (!is.null(player_to_start())) {
        removeUI("#player-start-selection")
        print("start_game with player")
        print(player_to_start())
      }
    })

    output$player <- renderText(player_to_start())
    # player to start selection
    # game with multiple legs and updating scoreboard
    # winner animation

  })
}
