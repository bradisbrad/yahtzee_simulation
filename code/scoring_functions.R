# Upper section ----
aces <- function(roll){
  sum(roll[which(roll == 1)])
}

twos <- function(roll){
  sum(roll[which(roll == 2)])
}

threes <- function(roll){
  sum(roll[which(roll == 3)])
}

fours <- function(roll){
  sum(roll[which(roll == 4)])
}

fives <- function(roll){
  sum(roll[which(roll == 5)])
}

sixes <- function(roll){
  sum(roll[which(roll == 6)])
}

upper_section <- function(roll){
  c(Aces = aces(roll),
    Twos = twos(roll),
    Threes = threes(roll),
    Fours = fours(roll),
    Fives = fives(roll),
    Sixes = sixes(roll),
    "Upper Bonus" = 0)
}

# Lower section ----
three_of_a_kind <- function(roll){
  roll_count <- table(roll)
  if(any(roll_count>=3)){
    sum(roll)
  } else {
    0
  }
}

four_of_a_kind <- function(roll){
  roll_count <- table(roll)
  if(any(roll_count>=4)){
    sum(roll)
  } else {
    0
  }
}

full_house <- function(roll){
  roll_count <- sort(table(roll))
  if(roll_count[1] == 2 & roll_count[2] == 3){
    25
    # } else if(roll_count[1] == 5) {
    #   25
  } else {
    0
  }
}

small_straight <- function(roll){
  if(!is.null(check_4(roll))){
    30
  } else {
    0
  }
}

large_straight <- function(roll){
  if(!is.null(check_5(roll))){
    40
  } else {
    0
  }
}

yahtzee <- function(roll){
  roll_count <- table(roll)
  if(roll_count[1] == 5){
    50
  } else {
    0
  }
}

chance <- function(roll){
  sum(roll)
}

lower_section <- function(roll){
  c("Three of a Kind" = three_of_a_kind(roll),
    "Four of a Kind" = four_of_a_kind(roll),
    "Full House" = full_house(roll),
    "Small Straight" = small_straight(roll),
    "Large Straight" = large_straight(roll),
    "Yahtzee" = yahtzee(roll),
    "Yahtzee Bonus" = 0,
    "Chance" = chance(roll))
}

# Bonus ----
upper_bonus <- function(card){
  if(sum(card$value[1:6], na.rm = T)>=63){
    card$value[7] <- 35
  }
  return(card)
}

yahtzee_bonus <- function(card, roll){
  yahtzee <- length(keep_mode(roll)) == 5
  yahtzee_scored <- !is.na(card$value[which(card$name == "Yahtzee")]) && 
    card$value[which(card$name == "Yahtzee")] == 50
  yahtzee_blanked <- !is.na(card$value[which(card$name == "Yahtzee")]) && 
    card$value[which(card$name == "Yahtzee")] == 0
  
  # Yahtzee Bonus if Yahtzee previously scored
  # Add 100 to yahtzee bonus
  if(yahtzee_scored){
    card$value[which(card$name == "Yahtzee Bonus")] <- 
      card$value[which(card$name == "Yahtzee Bonus")] + 100
  }
  
  # Joker rules apply even when Yahtzee has been blanked
  # Find where to score additional yahtzee
  ## Score in upper if upper is available
  upper_map <- c("Aces" = 1,
                 "Twos" = 2,
                 "Threes" = 3,
                 "Fours" = 4,
                 "Fives" = 5,
                 "Sixes" = 6)
  
  yahtzeed_upper <- names(upper_map[which(upper_map == roll[1])])
  
  upper_available <- is.na(card$value[which(card$name == yahtzeed_upper)])
  
  if(upper_available){
    card$value[which(card$name == yahtzeed_upper)] <- sum(roll)
    card$roll[which(card$name == yahtzeed_upper)] <- str_c(roll, collapse = "")
    return(card)
  }
  
  ## If upper unavailable, score in lower with special rules
  empty_scores <- card$name[which(is.na(card$value))]
  
  yahtzee_lower <- lower_section(roll)[which(!str_detect(names(lower_section(roll)), "Yahtzee"))] %>% 
    enframe() %>% 
    mutate(value = case_when(name == "Full House" ~ 25,
                             name == "Small Straight" ~ 30,
                             name == "Large Straight" ~ 40,
                             T ~ value),
           roll = str_c(!!roll, collapse = "")) %>% 
    filter(name %in% empty_scores) %>% 
    arrange(desc(value)) %>% 
    slice(1)
  
  if(nrow(yahtzee_lower) == 1){
    card <- card %>% 
      add_to_card(yahtzee_lower)
    return(card)
  }
  
  ## If Upper and lower unavailable, score 0 in lowest possible value upper section
  last_resort <- card %>% 
    filter(name %in% names(upper_map),
           is.na(value)) %>% 
    slice(1) %>% 
    mutate(value = 0,
           roll = str_c(!!roll, collapse = ""))
  
  card <- card %>% 
    add_to_card(last_resort)
  return(card)
}

# Full Scorecard ----
scorecard <- function(roll){
  c(upper_section(roll),
    lower_section(roll)) %>% 
    enframe() %>% 
    mutate(roll = str_c(!!roll, collapse = ""))
}


score <- function(card, roll, ...){
  empty_scores <- card$name[which(is.na(card$value))]
  
  apply_wt <- function(card, wt){
    card$wt[which(card$name == names(wt))] <- wt
    card
  }
  
  scored_card <- scorecard(roll) %>% 
    mutate(wt = 1)
  
  if(length(c(...))>0){
    wts <- c(...)
    for(i in 1:length(wts)){
      scored_card <- apply_wt(scored_card, wts[i])
    }
  }
  
  scored_card %>% 
    mutate(wtd_val = value * wt,
           empty = name %in% empty_scores) %>% 
    arrange(desc(wtd_val)) %>% 
    filter(empty) %>% 
    slice(1)
}

add_to_card <- function(card, score_to_add){
  
  card$value[which(card$name == score_to_add$name)] <- score_to_add$value
  card$roll[which(card$name == score_to_add$name)] <- score_to_add$roll
  card
}

update_scorecard <- function(card, roll, ...){
  empty_scores <- card$name[which(is.na(card$value))]
  
  if(yahtzee(roll) == 50 & !"Yahtzee" %in% empty_scores){
    card <- yahtzee_bonus(card, roll)
    card <- upper_bonus(card)
    return(card)
  }
  
  score_to_add <- score(card, roll, ...)
  
  card <- card %>% 
    add_to_card(score_to_add)
  
  card <- upper_bonus(card)
  return(card)
  
}
