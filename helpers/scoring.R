library(tidyverse)

legInit = function(startPoints = 301L, 
                    doubleOut = FALSE, 
                    playerNames = c("Leo", "Simon"), 
                    playerStart = 1L){
  
  score = 
    tibble(playerID = c(1L, 2L),
           score = rep(NA_integer_, 2), 
           scoreRemain = rep(startPoints, 2), 
           thrownDarts = rep(NA_integer_, 2), 
           thrownDartsTotal = rep(0L, 2), 
           round = rep(0L, 2))
  list(startPoints = startPoints, 
       doubleOut = doubleOut, 
       player1 = playerNames[playerStart], 
       player2 = playerNames[-playerStart], 
       nextPlayer = 1L, 
       playerWon = NULL, 
       score = score)
}


legAddScore = function(leg, score, darts = 3L){
  
  stopifnot(is.null(leg$playerWon), 
            is.integer(score), 
            is.integer(darts), 
            darts %in% c(1L, 2L, 3L), 
            score >= 0L, 
            score <= 180)
  
  oldScoreRemain = 
    leg$score %>% 
    filter(playerID == leg$nextPlayer) %>% 
    tail(1) %>% 
    pull(scoreRemain)
  
  newScoreRemain = oldScoreRemain - score
  
  oldThrownDartsTotal = 
    leg$score %>% 
    filter(playerID == leg$nextPlayer) %>% 
    tail(1) %>% 
    pull(thrownDartsTotal)
  
  newThrownDartsTotal = oldThrownDartsTotal + darts
  
  oldRound = 
    leg$score %>% 
    filter(playerID == leg$nextPlayer) %>% 
    tail(1) %>% 
    pull(round)
  
  newRound = oldRound + 1L
     
  
  # roll back to old score if new score is invalid
  if(newScoreRemain < 0L | (newScoreRemain == 1L & leg$doubleOut)){
    newScoreRemain = oldScoreRemain
    # newThrownDartsTotal = oldThrownDartsTotal
    score = 0L
    # darts = 0L
  }
  
  newScoreLine = 
    tibble(playerID = leg$nextPlayer, 
           score = score, 
           scoreRemain = newScoreRemain,
           thrownDarts = darts, 
           thrownDartsTotal = newThrownDartsTotal, 
           round = newRound)
  
  leg$score = bind_rows(leg$score, newScoreLine)
  
  if(newScoreRemain == 0L){
    leg$playerWon = leg$nextPlayer
    player_name = leg[[paste0("player", leg$nextPlayer)]]
    print(paste0(player_name, " won the leg after ", newThrownDartsTotal, " darts!"))
  }
  
  leg$nextPlayer = c(2L, 1L)[leg$nextPlayer]
  
  leg
}


# leg = legInit()
# 
# 
# leg = legAddScore(leg, 180L)
# leg = legAddScore(leg, 50L)
# leg = legAddScore(leg, 21L)
# leg = legAddScore(leg, 50L)
# leg = legAddScore(leg, 120L)
# leg = legAddScore(leg, 50L)
# leg = legAddScore(leg, 100L, 2L)
# 
# 
# leg = legAddScore(leg, 0)
# leg


legScoreboard = function(leg){
  scoreboard_raw = 
    leg$score %>% 
    left_join(tibble(playerID = c(1L, 2L), playerName = c(leg$player1, leg$player2)), 
              by = "playerID")
  
  total = 
    scoreboard_raw %>% 
    group_by(playerID) %>% 
    top_n(1, thrownDartsTotal) %>% 
    ungroup() %>% 
    select(playerID, scoreRemain)
  
  totals_row = tibble(total %>% filter(playerID == 1) %>% pull(scoreRemain), 
                      NA_integer_, 
                      total %>% filter(playerID == 2) %>% pull(scoreRemain))
  names(totals_row) = c(leg$player1, "round", leg$player2)
  
  if(nrow(scoreboard_raw) > 3){
    throws = 
      scoreboard_raw %>% 
      select(playerName, score, round) %>% 
      filter(round != 0L) %>% 
      spread(playerName, score) %>% 
      select(select_vars(include = c(leg$player1, "round", leg$player2)))
    
    totals_row = bind_rows(totals_row, throws)
  }
  
  totals_row
}

