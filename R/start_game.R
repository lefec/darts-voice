start_game <- function(session, players, sidebar_inputs, input, output, rv,
                       player_start = NULL) {

  if (length(players) > 3) {
    shinyWidgets::sendSweetAlert(session, "Error", "More than 3 players are not supported",
                                 type = "error")
    return(NULL)
  }

  removeUI("#game-winner-animation")

  purrr::walk(sidebar_inputs, shinyjs::disable)

  updateActionButton(session, "start_game", label = "Game started!")

  start_observers <<- list()

  updateTabItems(session, "main_body", "Game")

  purrr::walk(players, ~{
    .id_player <- glue::glue("player_starts_{.x}")
    .ui <- actionButton(glue::glue("player_starts_{.x}"), .x)
    insertUI("#starting-player", "beforeEnd", .ui)

    obs <- observeEvent(input[[.id_player]], {
      rv$game <- game_n_players$new(
        player_names = players,
        total_score  = input$total_score %>% as.numeric(),
        double_out   = input$double_out == "Double Out",
        best_of = input$best_of_x %>% as.numeric(),
        player_nr_to_start_first_leg = which(players == .x))

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
        actionButton("score", "Score") %>% shinyjs::hidden(),
        actionButton("score_rest", "Score Rest") %>% shinyjs::hidden()
      )

      insertUI("#content", "beforeBegin", .ui, immediate = TRUE)

      class_to_add <- glue::glue("dart-player{rv$leg$next_score}")
      shinyjs::addClass(class = class_to_add, selector = "#score_count")

      # trigger js function to focus the cursor on score_count
      session$sendCustomMessage(type = "focus-score", message = list(NULL))
    }, ignoreInit = TRUE)

    start_observers <<- c(start_observers, obs)
  })
}
