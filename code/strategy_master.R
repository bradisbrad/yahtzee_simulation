# The "It's All I've Got" Strategy ----
## simple_strategy_results <- sim_strat(simple_strategy, 100000)
simple_strategy <- function(card){
  while(!card_full(card)){
    roll <- roll_n(5)
    card <- update_scorecard(card, roll)
  }
  end_game(card)
}

# The "I Hate Yahtzee" Strategy ----
## fill_down_strategy_results <- sim_strat(fill_down_strategy, 100000)
fill_down_strategy <- function(card){
  while(!card_full(card)){
    empty_scores <- card$name[which(is.na(card$value))]
    roll <- roll_n(5)
    
    score_to_add <- scorecard(roll) %>% 
      filter(name == empty_scores[1])
    card <- add_to_card(card, score_to_add)
  }
  end_game(card)
}

# The "Yahtzee Superfan" Strategy ----
## keep_mode_strategy_results <- sim_strat(keep_mode_strategy, 100000)
keep_mode_strategy <- function(card){
  while(!card_full(card)){
    roll <- roll_n(5)
    for(i in 1:2){
      roll <- reroll(roll, keep_mode(roll))
    }
    card <- update_scorecard(card, roll)
  }
  end_game(card)
}



# The "Straight Some Chaser" Strategy ----
## run_yahtzee_strategy_results <- sim_strat(run_yahtzee_strategy, 100000)
run_yahtzee_strategy <- function(card){
  while(!card_full(card)){
    roll <- roll_n()
    
    if(nrow(card %>% filter(str_detect(name, "Straight"), is.na(value))) > 0){
      for(i in 1:2){
        if(length(keep_consecutive(roll)) >= 3){
          roll <- reroll_straight(roll)
        } else {
          roll <- reroll(roll, keep_mode(roll))
        }
      }
    } else {
      for(i in 1:2){
        roll <- reroll(roll, keep_mode(roll))
      }
    }
    card <- update_scorecard(card, roll, Chance = 1/3)
  }
  end_game(card)
}

# The Brute Force Strategy ----
## brute_force_strategy_results <- sim_strat(brute_force_strategy, 100000)
brute_force_strategy <- function(card){
  
  roll_options <- get_roll_options()
  keeper_combos <- get_keeper_combos()
  
  while(any(is.na(card$value))){
    (roll <- roll_n(5))
    for(i in 1:2){
      # print(roll)
      exps <- best_keepers(card, roll, roll_options, keeper_combos)
      keepers <- keeper_combos[[pull(slice(exps, 1), keeper_id)]]
      roll <- reroll(roll, keepers)
    }
    card <- update_scorecard(card, roll)
  }
  end_game(card)
}