plot_strat <- function(strat_res){
  strat_res %>% 
    group_by(game_id) %>% 
    summarize(final_score = mean(final_score)) %>% 
    ggplot() + 
    geom_histogram(aes(final_score)) +
    theme_bw()
}

strat_summary <- function(strat_res){
  strat_res %>% 
    mutate(name = factor(name, levels = start_game()$name)) %>% 
    group_by(name) %>% 
    summarize(mu = mean(value),
              sd = sd(value),
              blanked = sum(value == 0),
              avg_final = mean(final_score),
              sd_final = sd(final_score))
}

get_diff_ci <- function(strat1_results, strat2_results, pcnt){
  
  pcnt <- if(pcnt > 1) pcnt/100 else pcnt
  
  # Find min length
  min_len <- min(max(strat1_results$game_id), max(strat2_results$game_id))
  
  # Filter to same sample size
  strat1_results <- strat1_results %>% 
    filter(game_id <= min_len)
  
  strat2_results <- strat2_results %>% 
    filter(game_id <= min_len)
  
  t <- qt((1-pcnt)/2, min_len, lower.tail = F)
  
  ci_metrics <- strat1_results %>% 
    group_by(game_id) %>% 
    distinct(final_score) %>% 
    rename(strat1 = final_score) %>% 
    left_join(strat2_results %>% 
                group_by(game_id) %>% 
                distinct(final_score) %>% 
                rename(strat2 = final_score),
              by = c("game_id")) %>% 
    ungroup() %>% 
    mutate(diff = strat1 - strat2,
           dbar = mean(diff),
           sqer = (diff - dbar)^2) %>% 
    summarize(dbar = mean(dbar),
              ssd = sum(sqer)/(min_len-1))
  
  ci_pcnt <- t*sqrt(ci_metrics$ssd/min_len)
  return(c(round(ci_metrics$dbar - ci_pcnt, 2), round(ci_metrics$dbar + ci_pcnt, 2)))
}

