if(!require("tidyverse")) install.packages("tidyverse")
if(!require("here")) install.packages("here")
if(!require("furrr")) install.packages("furrr")

library(tidyverse)
library(here)
library(furrr)

for(i in list.files(here("code"), full.names = T)){
  source(i)
}

for(i in list.files(here("sim_results"), full.names = T)){
  if(str_detect(i, ".R")) source(i)
}
