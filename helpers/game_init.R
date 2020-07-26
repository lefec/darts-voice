i_box <- function(leg, input, output, .width) {
  info_id  <- glue("remain_{leg$player_name}")
  table_id <- glue("table_{leg$player_name}")
  
  # server
  output[[info_id]] <- 
    renderInfoBox(
      infoBox(leg$player_name, leg$remain(), subtitle = "", 
              icon = icon("bullseye")))
  
  output[[table_id]] <- DT::renderDataTable(leg$getHist())
  
  # ui
  .ui <- column(
    width = .width, 
    div(style = glue("border-color: {leg$player_color}; border-width: 15px; 
                     border-style: solid; padding: 10px"),
        infoBoxOutput(info_id, 12), 
        DT::dataTableOutput(table_id),
        )
  )
  
  insertUI("#content", "beforeEnd", .ui)
  
}
