simple_strategy_results <- read_csv(here("sim_results/simple_strategy_results-100000.csv"))
fill_down_strategy_results <- read_csv(here("sim_results/fill_down_strategy_results-100000.csv"))
keep_mode_strategy_results <- read_csv(here("sim_results/keep_mode_strategy_results-100000.csv"))
run_yahtzee_strategy_results <- read_csv(here("sim_results/run_yahtzee_strategy_results-100000.csv"))
brute_force_strategy_results <- read_csv(here("sim_results/brute_force_strategy_results-1000.csv"))


comp_table <- tibble(`Strategy 1` = c("Brute Force", "Brute Force", "Run Yahtzee", "Run Yahtzee", "Simple Strategy"),
       `Strategy 2` = c("Run Yahtzee", "Keep Mode", "Keep Mode", "Simple Strategy", "Fill Down"),
       `95% CI for Strategy 1 vs. Strategy 2` = c(
         paste0("[", str_c(as.character(
           get_diff_ci(brute_force_strategy_results, run_yahtzee_strategy_results, pcnt = 95)), 
           collapse = ", "), "]"),
         paste0("[", str_c(as.character(
           get_diff_ci(brute_force_strategy_results, keep_mode_strategy_results, pcnt = 95)), 
           collapse = ", "), "]"),
         paste0("[", str_c(as.character(
           get_diff_ci(run_yahtzee_strategy_results, keep_mode_strategy_results, pcnt = 95)), 
           collapse = ", "), "]"),
         paste0("[", str_c(as.character(
           get_diff_ci(run_yahtzee_strategy_results, simple_strategy_results, pcnt = 95)), 
           collapse = ", "), "]"),
         paste0("[", str_c(as.character(
           get_diff_ci(simple_strategy_results, fill_down_strategy_results,pcnt = 95)), 
           collapse = ", "), "]")
       ))


comp_table_img <- comp_table %>% 
  kableExtra::kbl() %>% 
  kableExtra::kable_classic()
