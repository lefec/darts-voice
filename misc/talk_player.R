talk_player = function(var, player_nr, player_name, voice_name = NULL){
  # renderUI({
    # to prevent browser caching, create a new audio filename each play
    # create within the www folder of the Shiny app
    output_file <- paste0("www/player", player_nr, "_", isolate(var), ".wav")
    
    # replace with your reactive text input
    gl_talk(paste(player_name, var),
            output = output_file, 
            name = voice_name)
    
    # creates HTML5 audio player
    # the audio file sits in folder www, but the audio file must be referenced without www
    tags$audio(autoplay = NA, controls = NA, tags$source(src = basename(output_file)))
  # })
}

# talk_player("asd", 1, "simon")
