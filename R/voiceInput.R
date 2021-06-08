# stolen from https://www.jumpingrivers.com/blog/voice-control-your-shiny-apps/


# I have used the keyword jarvis as default because I like iron man
# default value is an empty string
# necessary to choose an "inputId" to bind the value to
voiceInput = function(inputId, keyword = "jarvis", value = "") {
  tagList(
    singleton(
      tags$head(
        tags$script(src = "//cdnjs.cloudflare.com/ajax/libs/annyang/2.6.0/annyang.min.js"),
        tags$script(HTML("annyang.start({ autoRestart: true, continuous: true});")))),
    tags$head(tags$script(HTML(
      glue::glue("var <inputId>  = '';
            if(annyang){
              var commands = {
                '<keyword> *val': function(val) {
                  <inputId> = val;
                  Shiny.onInputChange('<inputId>', <inputId>);
                }
              }
              annyang.addCommands(commands);
            }", .open = "<", .close = ">")
    )
    )
    )
  )
}