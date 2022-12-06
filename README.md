# Yahtzee Simulation Project

## Scripts

All the scripts needed for running a Yahtzee simulation strategy are included in the `code` directory

-   `code/dice_functions.R` includes functions needed for rolling/rerolling the dice, as well as a few helper functions for checking dice values and keeping certain values/runs/frequencies of dice

-   `code/scoring_functions.R` includes functions needed for determining the scores of a given roll as well as creating/updating scorecards during a game - `code/gamesim_functions.R` includes the functions needed to simulate a game of Yahtzee, as well as functions needed to simulate large numbers of Yahtzee games in parallel

-   `code/brute_force_functions.R` includes functions needed to carry out the Brute Force strategy implemented towards the end of the Yahtzee Strategy Performance Evaluation report

-   `code/strategy_analysis_functions.R` includes a few helper functions to quickly summarize and plot simulation results. This script also includes an implementation of Paired-*t* confidence interval calculation

-   `code/strategy_master.R` includes all strategies discussed in the Yahtzee Strategy Performance Evaluation report, as well as a comment above each one showing how to run a 100000 game simulation of the strategy. Note: running a 100000 game simulation of the Brute Force strategy will set your computer on fire, proceed with caution

## Data

All strategy simulation results can be found in the `sim_results` directory. All data files are .csv files with the naming format `[STRATEGY]_results-[NUMBER OF RUNS].csv`. This directory also includes a script, `sim_results/strategy_comparison.R` that creates the comparison table used later in the Yahtzee Strategy Performance Evaluation report and also acts as a good example on how to read in simulation results and use the paired CI calculation function.

## Initiating Project

The topline `runtime.R` function should populate your environment with all functions and results. If any libraries are missing, please install them before continuing. If any functions are missing, sourcing the scripts individually in any order should be fine.

## Workflow

To run the Simple Strategy, the steps are as follows:

-   Run `runtime.R`

-   To play a single game,

    -   `play_strat(1, simple_strategy)`

-   To simulate 10 games

    -   `sim_strat(simple_strategy, 10)`

    -   If you have problems running in parallel

        -   `sim_strat(simple_strategy, 10, parallel = F)`

-   To compare two strategies

    -   `get_diff_ci(simple_strategy_results, fill_down_strategy_results, pcnt = 90)`

        -   This function should take either integer or decimal percentage format
