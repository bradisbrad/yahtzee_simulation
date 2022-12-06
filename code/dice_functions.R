# Dice Roll ----
roll_n <- function(n = 5){
  sample(1:6, n, replace = T)
}

# Reroll ----
reroll <- function(roll, keep = NULL){
  kept <- roll[keep]
  roll <- roll_n()
  roll[keep] <- kept
  roll
}

# Keep Helpers ----
keep_pips <- function(roll, pips){
  which(roll %in% pips)
}

keep_unique_pips <- function(roll, pips){
  keepers <- c()
  for(i in 1:length(pips)){
    if(any(roll == pips[i])){
      keepers[i] <- which(roll == pips[i])[1]
    }
  }
  keepers
}

keep_top_n <- function(roll, n){
  pips <- rev(sort(unique(roll)))[1:n]
  keep_pips(roll, pips)
}

keep_mode <- function(roll){
  roll_count <- table(roll)
  max_mode <- max(as.integer(names(roll_count[which(roll_count == max(roll_count))])))
  keep_pips(roll, max_mode)
}

# Check Functions ----
check_5 <- function(roll, print_msg = F){
  roll <- setNames(roll, 1:5)
  diffs <- diff(sort(roll))
  rle_diffs <- rle(diffs)
  con_len <- rle_diffs$lengths[which(rle_diffs$values == 1)]
  if(length(con_len) == 1 && con_len == 4){
    if(print_msg) print("Large Straight Exists: Keep all")
    return(as.integer(names(roll)))
  }
}

check_4 <- function(roll, print_msg = F){
  roll <- setNames(roll, 1:5)
  sorted_roll <- sort(roll)
  usort_roll <- unique(sort(roll))
  diffs <- diff(sorted_roll)
  usort_diffs <- diff(usort_roll)
  rle_diffs <- rle(usort_diffs)
  con_len <- rle_diffs$lengths[which(rle_diffs$values == 1)]
  if(any(con_len == 3)){
    if (print_msg) print("Small straight exists: keep 4 dice")
    mid_pip <- usort_roll[which(usort_roll - lead(usort_roll) == -1 & 
                                  usort_roll - lag(usort_roll) == 1)]
    
    pips <- unique(c(mid_pip-1, mid_pip, mid_pip+1))
    keepers <- keep_unique_pips(roll, pips)
    return(keepers)
  }
}

check_3 <- function(roll, print_msg = F){
  roll <- setNames(roll, 1:5)
  sorted_roll <- sort(roll)
  usort_roll <- unique(sort(roll))
  diffs <- diff(sorted_roll)
  usort_diffs <- diff(usort_roll)
  rle_diffs <- rle(usort_diffs)
  con_len <- rle_diffs$lengths[which(rle_diffs$values == 1)]
  if(any(con_len == 2)){
    if (print_msg) print("3 dice run exists: keep 3 dice")
    mid_pip <- usort_roll[which(usort_roll - lead(usort_roll) == -1 & 
                                  usort_roll - lag(usort_roll) == 1)]
    
    pips <- unique(c(mid_pip-1, mid_pip, mid_pip+1))
    keepers <- keep_unique_pips(roll, pips)
    return(keepers)
  }
}

check_2 <- function(roll, hook = "high", print_msg = F){
  roll <- setNames(roll, 1:5)
  sorted_roll <- sort(roll)
  diffs <- diff(sort(roll))
  rle_diffs <- rle(diffs)
  con_len <- rle_diffs$lengths[which(rle_diffs$values == 1)]
  if(any(con_len == 1)){
    if (print_msg) print("2 dice run exists: keep 2 dice")
    if(hook == "high"){
      if (print_msg) print("keeping highest run (if applicable)")
      return(as.integer(names(sorted_roll[max(which(diffs==1)):(max(which(diffs==1))+1)])))
    } else if (hook == "low"){
      if (print_msg) print("keeping lowest run (if applicable)")
      return(as.integer(names(sorted_roll[min(which(diffs==1)):(min(which(diffs==1))+1)])))
    } 
  }
}


keep_consecutive <- function(roll, hook = "high"){
  c5 <- check_5(roll)
  if(!is.null(c5)){
    return(c5)
  }
  
  c4 <- check_4(roll)
  if(!is.null(c4)){
    return(c4)
  }
  
  c3 <- check_3(roll)
  if(!is.null(c3)){
    return(c3)
  }
  
  c2 <- check_2(roll)
  if(!is.null(c2)){
    return(c2)
  }
  
  #print("no consecutive dice")
  return(NULL)
}

# Reroll Helpers ----
reroll_pips <- function(roll, pips){
  reroll(roll, keep_pips(roll, pips))
}

reroll_straight <- function(roll){
  reroll(roll, keep_consecutive(roll))
}