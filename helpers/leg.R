reactiveTrigger <- function() {
  counter <- reactiveVal(0)
  list(
    depend = function() {
      counter()
      invisible()
    },
    trigger = function() {
      counter( isolate(counter()) + 1 )
    }
  )
}
# https://gist.github.com/bborgesr/3350051727550cfa798cb4c9677adcd4
Leg1Player <- R6Class(
  classname = "Leg1Player", 
  public = list(
    player_name = NA, 
    total_score = NA_real_,
    double_out = FALSE,
    player_color = NA_character_,
    
    # hist = list(),
    
    initialize = function(player_name, color, total_score = 301, 
                          double_out = FALSE, reactive = TRUE){
      stop_if_not(player_name, is.character)
      stop_if_not(player_name, ~length(.) == 1)
      
      stop_if_not(total_score, is.numeric)
      stop_if_not(total_score, ~ . %in% c(301, 501))
      
      stop_if_not(double_out, is.logical)
      
      self$player_name  <- player_name
      self$total_score  <- total_score
      self$double_out   <- double_out
      self$player_color <- color
      private$hist <- tibble(score = NA_real_, darts_count = NA_real_, round = 0L, 
                          remaining = total_score)
      private$rxTrigger = reactiveTrigger()
      private$reactive = reactive
    }, 
    
    print = function(...){
      cat("Leg:", self$status(), "| Player name:", self$player_name, 
          "| Goal:", self$total_score, "|", 
          if_else(self$double_out, "Double", "Single"), "out \n", sep="\t")
      
      invisible(self)
    }, 
    
    remain = function(){
      if(private$reactive) private$rxTrigger$depend()
      private$hist$remaining %>% tail(1)
    },
    
    round = function(){
      if(private$reactive) private$rxTrigger$depend()
      private$hist$round %>% tail(1)
    },
    
    status = function(){
      case_when(
        self$round() == 0L ~ "not started", 
        self$remain() == 0 ~ "finished", 
        TRUE ~ "ongoing"
      )
    },
    
    score = function(value){
      stop_if_not(value, is.numeric)
      stop_if_not(value, ~ . >= 0)
      stop_if_not(value, ~ . <= 180)
      
      # check if score is valid
      if(value > self$remain()){
        value <- 0
      }
      
      if (private$reactive) private$rxTrigger$trigger()
      
      hist_tmp <- 
        tibble(score = value, darts_count = 3, 
               round = self$round() + 1L, 
               remaining = self$remain() - value)
      
      private$hist <- bind_rows(private$hist, hist_tmp)
    },
    
    score_rest = function(rest){
      value <- self$remain() - rest
      self$score(value)
    },
     
    getHist = function(){
      if(private$reactive) private$rxTrigger$depend()
      private$hist %>% 
        select(round, score, remaining) %>% 
        arrange(desc(round)) %>% 
        datatable(
          rownames = FALSE, 
          selection = 'none', 
          autoHideNavigation = TRUE, 
          fillContainer = FALSE, 
          options = list(
            ordering = FALSE, 
            searching = FALSE, 
            info = FALSE, 
            lengthChange = FALSE, 
            paging = FALSE
          )
        )
    }
  ), private = list(
    hist = tibble(),
    rxTrigger = NULL, 
    reactive = NULL
  ))

LegNPlayers <- R6Class(
  "LegNPlayers", 
  public = list(
    legs = list(),
    players = NA_character_,
    next_score = 1, 
    has_finished = FALSE,
    player_colors = c('#66c2a5','#fc8d62','#8da0cb','#e78ac3'),
    
    initialize = function(player_names, total_score = 301, double_out = FALSE){
      stop_if_not(player_names, is.character)
      stop_if_not(player_names, ~ length(.) >= 1)
      
      stop_if_not(total_score, is.numeric)
      stop_if_not(total_score, ~ . %in% c(301, 501))
      
      stop_if_not(double_out, is.logical)
      
      self$player_colors <- self$player_colors[seq_along(player_names)]
      legs <- map2(player_names, self$player_colors, 
                  Leg1Player$new, 
                  total_score = total_score, double_out = double_out)
      names(legs) <- player_names
      
      self$legs <- legs
      self$players <- player_names
    }, 
    
    print = function(...) {
      cat("Players", self$players, "| Has finished: ", self$has_finished)
      invisible(self)
    }, 
    
    score = function(value){
      stop_if_not(self$has_finished, isFALSE)
      
      self$legs[[self$next_score]]$score(value)
      
      if(self$legs[[self$next_score]]$status() == "finished"){
        self$has_finished <- TRUE
        print("finished")
      }
      
      self$next_score <- if_else(self$next_score == length(self$players), 1, self$next_score + 1)
    }, 
    
    score_rest = function(value){
      stop_if_not(self$has_finished, isFALSE)
      
      self$legs[[self$next_score]]$score_rest(value)
      
      if(self$legs[[self$next_score]]$status() == "finished"){
        self$has_finished <- TRUE
        print("finished")
      }
      
      self$next_score <- if_else(self$next_score == length(self$players), 1, self$next_score + 1)
    }
  )
)


# p1 <- Leg1Player$new("Leo", 301)
# p1$print()
# p1$hist
# p1$score(131)
# p1$hist
# p1$remain()

