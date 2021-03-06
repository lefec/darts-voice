i_box <- function(leg, input, output, .width) {
  info_id  <- glue::glue("remain_{leg$player_name}")
  table_id <- glue::glue("table_{leg$player_name}")

  # server
  output[[info_id]] <-
    renderInfoBox(
      infoBox(leg$player_name, leg$remain(), subtitle = "",
              icon = icon("bullseye")))

  output[[table_id]] <- DT::renderDataTable(leg$getHist())

  # ui
  .ui <- column(
    width = .width,
    div(style = glue::glue("border-color: {leg$player_color}; border-width: 15px;
                     border-style: solid; padding: 10px"),
        infoBoxOutput(info_id, 12),
        DT::dataTableOutput(table_id),
        )
  )

  insertUI("#content", "beforeEnd", .ui)
}

get_player_names <- function(input_names) {
    stringr::str_split(input_names, ",", simplify = FALSE)[[1]] %>%
    stringr::str_trim()
}
