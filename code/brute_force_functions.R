## Best Options - Brute Force ----
get_roll_options <- function(){
  expand.grid(1:6, 1:6, 1:6, 1:6, 1:6) %>% 
    as_tibble() %>% 
    rename(d1 = Var1,
           d2 = Var2,
           d3 = Var3,
           d4 = Var4,
           d5 = Var5) %>% 
    rowwise() %>% 
    mutate(c_roll = list(c(d1, d2, d3, d4, d5))) %>% 
    ungroup() %>% 
    mutate(`Yahtzee` = map_dbl(c_roll, yahtzee),
           `Large Straight` = map_dbl(c_roll, large_straight),
           `Small Straight` = map_dbl(c_roll, small_straight),
           `Full House` = map_dbl(c_roll, full_house),
           `Four of a Kind` = map_dbl(c_roll, four_of_a_kind),
           `Three of a Kind` = map_dbl(c_roll, three_of_a_kind),
           `Chance` = map_dbl(c_roll, chance),
           `Sixes` = map_dbl(c_roll, sixes),
           `Fives` = map_dbl(c_roll, fives),
           `Fours` = map_dbl(c_roll, fours),
           `Threes` = map_dbl(c_roll, threes),
           `Twos` = map_dbl(c_roll, twos),
           `Aces` = map_dbl(c_roll, aces))
}

get_keeper_combos <- function(){
  suppressWarnings(
    as_tibble(t(data.frame(combn(5, 5)))) %>% 
      rowwise() %>% 
      mutate(dice_to_keep = list(c(V1, V2, V3, V4, V5))) %>% 
      bind_rows(
        as_tibble(t(data.frame(combn(5, 4)))) %>% 
          rowwise() %>% 
          mutate(dice_to_keep = list(c(V1, V2, V3, V4)))
      ) %>% 
      bind_rows(
        as_tibble(t(data.frame(combn(5, 3)))) %>% 
          rowwise() %>% 
          mutate(dice_to_keep = list(c(V1, V2, V3)))
      ) %>% 
      bind_rows(
        as_tibble(t(data.frame(combn(5, 2)))) %>% 
          rowwise() %>% 
          mutate(dice_to_keep = list(c(V1, V2)))
      ) %>% 
      bind_rows(
        as_tibble(t(data.frame(combn(5, 1)))) %>% 
          rowwise() %>% 
          mutate(dice_to_keep = list(c(V1)))
      ) %>% 
      ungroup() %>% 
      pull(dice_to_keep)
  )
}

check_dice <- function(card, roll, roll_options, dice_to_keep){
  empty_scores <- card$name[which(is.na(card$value))]
  # Leave scored yahtzees in expectation for bonus
  if(!"Yahtzee" %in% empty_scores && 
     card$value[which(card$name == "Yahtzee")] == 50) empty_scores <- c("Yahtzee", empty_scores)
  for (i in dice_to_keep){
    roll_options <- roll_options %>% 
      filter(.[[i]] == roll[i])
  }
  roll_options %>% 
    summarize(across(Yahtzee:Aces, mean)) %>% 
    select_if(names(.) %in% empty_scores)
}

best_keepers <- function(card, roll, roll_options = roll_options, 
                         keeper_combos = keeper_combos){
  #plan(strategy = multisession, workers = 8)
  map_dfr(keeper_combos, ~check_dice(card, roll, roll_options, .x), 
          .id = "keeper_id") %>% 
    group_by(keeper_id) %>% 
    mutate(exp = sum(across(everything()))) %>% 
    ungroup() %>% 
    mutate(keeper_id = as.integer(keeper_id)) %>% 
    arrange(desc(exp))
}