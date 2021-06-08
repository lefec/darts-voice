app <- function() {
  ui <- fluidPage(
    match_ui("match")
  )

  server <- function(input, output, session) {
    match_server(
      id = "match",
      match_settings = list(
        player_names                 = c("Leo", "Simon"),
        double_out                   = FALSE,
        total_score                  = 301,
        best_of                      = 1,
        player_nr_to_start_first_leg = 2
      ))
  }

  shinyApp(ui, server)
}
