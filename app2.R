library(shiny)
library(googleLanguageR)
library(DT)
gl_auth("gl_auth.json")

source("helpers/scoring.R")

ui = 
  fluidPage(
    singleton(tags$head(
      tags$script(src="//cdnjs.cloudflare.com/ajax/libs/annyang/2.6.1/annyang.min.js"),
      includeScript('init.js')
    )),
    textOutput('foo'),
    uiOutput('talk_player1'), 
    uiOutput('talk_player2'), 
    numericInput('newScore', "Enter Score:", value = 0, min = 0, max = 180),
    actionButton('confirmScore', "Confirm Score"),
    DT::dataTableOutput('scoreboard')
  )

server = function(input, output) {
  
  rv = reactiveValues()
  rv$leg = legInit()
  
  output$foo = renderText({
    paste0('Leo: ', input$player1, '; Simon: ', input$player2)
  })
  
  observeEvent(input$confirmScore, 
               {
                 rv$leg = legAddScore(rv$leg, isolate(input$newScore))
               })
  
  output$scoreboard = DT::renderDataTable(legScoreboard(rv$leg))
  
  output$talk_player1 <- renderUI({
    
    # to prevent browser caching, create a new audio filename each play
    # create within the www folder of the Shiny app
    output_file <- paste0("www/player1_", isolate(input$player1), ".wav")
    
    # replace with your reactive text input
    gl_talk(paste0("Leo ", input$player1),
            output = output_file)
    
    # creates HTML5 audio player
    # the audio file sits in folder www, but the audio file must be referenced without www
    tags$audio(autoplay = NA, controls = NA, tags$source(src = basename(output_file)))
    
  })
  
  output$talk_player2 <- renderUI({
    output_file <- paste0("www/player2_", isolate(input$player2), ".wav")
    gl_talk(paste0("Simon ", input$player2),
            output = output_file, 
            # name = "en-AU-Wavenet-B")
            name = "en-US-Wavenet-F")
    tags$audio(autoplay = NA, controls = NA, tags$source(src = basename(output_file)))
  })
  
}

shinyApp(ui, server)
