reactive_trigger <- function() {
  counter <- reactiveVal(0)
  list(
    depend = function() {
      counter()
      invisible()
    },
    trigger = function() {
      counter(isolate(counter()) + 1)
    }
  )
}

# https://gist.github.com/bborgesr/3350051727550cfa798cb4c9677adcd4
# leg_1_player --------------------------------------------------------------
leg_1_player <- R6Class(
  classname = "leg_1_player",
  public = list(
    player_name = NA,
    total_score = NA_real_,
    double_out = FALSE,
    player_color = NA_character_,
    valid_score = TRUE,

    initialize = function(player_name, color, total_score = 301,
                          double_out = FALSE, reactive = TRUE) {
      stop_if_not(player_name, is.character)
      stop_if_not(player_name, ~length(.) == 1)

      stop_if_not(total_score, is.numeric)
      stop_if_not(total_score, ~ . %in% c(301, 501))

      stop_if_not(double_out, is.logical)

      self$player_name   <- player_name
      self$total_score   <- total_score
      self$double_out    <- double_out
      self$player_color  <- color
      private$rx_trigger <- reactive_trigger()
      private$reactive   <- reactive
      
      private$hist <-
        tibble(score = NA_real_, 
               darts_count = NA_real_, 
               round = 0L, 
               remaining = total_score, 
               date = Sys.time())
      
    },
    
    print = function(...) {
      cat("Leg:", self$status(), "| Player name:", self$player_name, 
          "| Goal:", self$total_score, "|", 
          if_else(self$double_out, "Double", "Single"), "out \n", sep = "\t")
      
      invisible(self)
    }, 
    
    remain = function() {
      if (private$reactive) private$rx_trigger$depend()
      private$hist$remaining %>% tail(1)
    },
    
    round = function() {
      if (private$reactive) private$rx_trigger$depend()
      private$hist$round %>% tail(1)
    },
    
    status = function() {
      case_when(
        self$round() == 0L ~ "not started", 
        self$remain() == 0 ~ "finished", 
        TRUE ~ "ongoing"
      )
    },
    
    score = function(value) {
      value_checks <- c(is.numeric(value), between(value, 0, 180))
      
      if (all(value_checks)) {
        
        # check if score is valid
        if (value > self$remain()) {
          value <- 0
        }
        
        if (private$reactive) private$rx_trigger$trigger()
        
        hist_tmp <- 
          tibble(score = value, darts_count = 3, 
                 round = self$round() + 1L, 
                 remaining = self$remain() - value, 
                 date = Sys.time())
        
        private$hist <- bind_rows(private$hist, hist_tmp)
        self$valid_score <- TRUE
      } else {
        self$valid_score <- FALSE
      }
      
    },
    
    score_rest = function(rest) {
      value <- self$remain() - rest
      self$score(value)
    },
     
    getHist = function() {
      if (private$reactive) private$rx_trigger$depend()
      private$hist %>% 
        select(round, score, remaining) %>% 
        arrange(desc(round)) %>% 
        datatable(
          rownames = FALSE, 
          selection = "none", 
          autoHideNavigation = TRUE, 
          fillContainer = FALSE, 
          options = list(
            info         = FALSE,
            lengthChange = FALSE,
            ordering     = FALSE,
            paging       = FALSE,
            searching    = FALSE 
          )
        )
    }, 
    
    saveHist = function() {
      .game_id <- uuid::UUIDgenerate()
      
      hist_to_save <- 
        private$hist %>% 
        mutate(game_id = .game_id, 
               leg_id = 1L) %>% 
        select(game_id, leg_id, everything())
      
      game_to_save <- 
        tibble(
          game_id = .game_id,
          leg_id = 1L, 
          player_name = self$player_name, 
          won = self$remain() == 0,
          double_out = self$double_out, 
          total_score = self$total_score, 
          date_end = private$hist$date %>% tail(1)
        )
      
      readr::write_csv(hist_to_save, "data/hist.csv", append = TRUE)
      readr::write_csv(game_to_save, "data/leg.csv", append = TRUE)
    }
    
  ), private = list(
    hist = tibble(),
    rx_trigger = NULL, 
    reactive = NULL
  ))

# leg_n_players --------------------------------------------------------------
leg_n_players <- R6Class(
  "leg_n_players", 
  public = list(
    legs = list(),
    players = NA_character_,
    next_score = 1, 
    has_finished = NA_character_,
    player_colors = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3"),
    player_start = NA_real_, 
    
    initialize = function(player_names, total_score = 301, double_out = FALSE, 
                          reactive = TRUE, player_start = 1) {
      stop_if_not(player_names, is.character)
      stop_if_not(player_names, ~ length(.) >= 1)
      
      stop_if_not(total_score, is.numeric)
      stop_if_not(total_score, ~ . %in% c(301, 501))
      
      stop_if_not(double_out, is.logical)
      
      self$player_colors <- self$player_colors[seq_along(player_names)]
      legs <- map2(player_names, self$player_colors, 
                  leg_1_player$new, 
                  total_score = total_score, double_out = double_out)
      names(legs) <- player_names
      
      self$next_score   <- player_start
      self$player_start <- player_start
      self$legs         <- legs
      self$players      <- player_names
      
      private$rx_trigger <- reactive_trigger()
      private$reactive   <- reactive
    }, 
    
    print = function(...) {
      cat("Players", self$players, "| Has finished: ", self$has_finished)
      invisible(self)
    }, 
    
    score = function(value, rest) {
      stop_if_not(self$has_finished, is.na)
      
      scoring_fun <- if (rest) {
        self$legs[[self$next_score]]$score_rest 
      } else {
        self$legs[[self$next_score]]$score
      }
      
      scoring_fun(value)
      
      # checks the "valid_score" argument. Only proceed to the next player
      # if the score was valid.
      if (self$legs[[self$next_score]]$valid_score) {
        
        if (self$legs[[self$next_score]]$status() == "finished") {
          self$has_finished <- self$players[self$next_score]
        }
        
        self$next_score <- 
          if_else(self$next_score == length(self$players), 
                  1, self$next_score + 1)
      }
      
      # return logical value if the scoring was successful or not  
      self$legs[[self$next_score]]$valid_score
    }, 
    
    is_finished = function(name) {
      if (private$reactive) private$rx_trigger$depend()
      self$has_finished <- name
    }
  ), 
  private = list(
    rx_trigger = NULL, 
    reactive = NULL
  )
)


# game_n_players -------------------------------------------------------------
game_n_players <- R6Class(
  "game_n_players", 
  public = list(
    
    game_hist    = tibble(),
    leg          = list(),
    leg_id       = 1,
    player_names = NA_character_,
    total_score  = 0,
    double_out   = FALSE,
    best_of      = 0,
    player_nr_to_start_first_leg = NA_real_,

    
    
    initialize = function(player_names,
                          total_score = 301,
                          double_out = FALSE,
                          best_of = 3, 
                          player_nr_to_start_first_leg = 1) {
      
      self$game_hist <- tibble(
        leg = seq_len(best_of),
        winner = rep(NA_character_, best_of))
      
      
      self$player_names <- player_names
      self$total_score  <- total_score
      self$double_out   <- double_out
      self$best_of      <- best_of
      self$player_nr_to_start_first_leg <- player_nr_to_start_first_leg
      
      private$rx_trigger <- reactive_trigger()
      private$reactive   <- TRUE
      
      self$leg <- leg_n_players$new(
            player_names = player_names,
            total_score  = total_score,
            double_out   = double_out, 
            player_start = player_nr_to_start_first_leg)
      
      
    }, 
    
    player_won = function(winning_player) {
      self$game_hist$winner[self$leg_id] <- winning_player
      self$leg_id <- self$leg_id + 1
      walk(self$leg$legs, ~ .x$saveHist())
      
      if (self$leg_id <= self$best_of) {
        
        next_starting_player <- self$leg$player_start + 1
        next_starting_player <- 
          if_else(next_starting_player > length(self$player_names), 
                  1, next_starting_player)
        
        self$leg <- leg_n_players$new(
          player_names = self$player_names,
          total_score  = self$total_score,
          double_out   = self$double_out, 
          player_start = next_starting_player)
      }
      
      invisible(self)
    }, 
    
    scoreboard = function() {
      # still need to work out which of these reactive triggers is
      # actually doing something and remove them accordingly
      if (private$reactive) private$rx_trigger$depend()
      
      join_tib <- tibble(winner = self$player_names)
      
      self$game_hist %>% 
        filter(!is.na(winner)) %>% 
        count(winner, sort = TRUE) %>% 
        full_join(join_tib, by = "winner") %>% 
        replace_na(list(n = 0))
    }, 
    
    scoreboard_dt = function() {
      self$scoreboard() %>%
        rename("Player" = "winner", "Legs won" = "n") %>% 
        datatable(rownames = FALSE, selection = "none", options = list(
          paging = FALSE, 
          info = FALSE, 
          searching = FALSE, 
          ordering = FALSE
      ))
    }, 
    
    is_won = function() {
      legs_to_win <- ceiling(self$best_of / 2)
      self$scoreboard()$n[1] == legs_to_win
    }, 
    
    get_leg_id = function() {
      glue("Leg: {self$leg_id}")
    }
  ), 
  private = list(
    rx_trigger = NULL, 
    reactive = NULL
  )
)
