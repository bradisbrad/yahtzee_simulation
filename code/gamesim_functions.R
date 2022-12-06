## Game Simulation ----
start_game <- function(){
  scorecard(c(1, 1, 1, 1, 1)) %>% 
    mutate(value = case_when(str_detect(name,  " Bonus") ~ 0),
           roll = NA)
}

play_strat <- function(game_id, strat){
  game <- {{strat}}(start_game())
  score <- game$score
  card <- game$card
  card$final_score <- score
  card$game_id <- game_id
  card
}

sim_strat <- function(strat, n, parallel = T){
  plan(multisession, workers = parallel::detectCores()-1)
  cards <- c()
  if(parallel){
    future_map_dfr(1:n, play_strat, strat = {{strat}}, 
                   .options = furrr_options(seed = 1))
  } else if (!parallel){
    map_dfr(1:n, play_strat, strat = {{strat}})
  }
}

card_full <- function(card){
  !any(is.na(card$value))
}

end_game <- function(card){
  score <- sum(card$value)
  return(list("score" = score, 
              "card" = card))
}

