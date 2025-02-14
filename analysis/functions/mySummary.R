# MySummary- Function
# by Christine Nussbaum - 04.2020


##########################################################################################
# input: data frame, dependent variable, all desired grouping variables

mySummary <- function(df,dv, ...){
  dv <- enquo(dv)             # bring dv in right format
  groupvar <- quos(...)       # bring groupvariables in right format
  # help: https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html
  
  # Group dataset and calculate Mean, SD, N, Standard error and CI-Whisker
  df %>% group_by(!!! groupvar) %>% summarise(M = mean(!! dv),
                                              SD = sd(!! dv), 
                                              N = length(!! dv),
                                              SE = SD/sqrt(N), 
                                              CI = SE* qt(0.95/2 + .5, N-1)) %>%
    ungroup -> df
  
  df<- df %>% rename(!! dv := M)   # Rename the M into the original dependent Variable
}

