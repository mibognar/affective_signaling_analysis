# Use this function to detect participant and trial outliers
# With the parameters you can define the sd cutoff limit, and the investigated conditions
# Always define participant ID as the first parameter in params
# Example usage: outlier_detection(test_df, 3, 3, participant_id, is_congruent, is_prev_congruent)
outlier_detection = function(mydataframe, trial_sd_cut=2.5, participant_sd_cut=2.5, ...) {
  dots = as.list(substitute(list(...)))[-1L]
  myparams = paste(dots)
  rt_participant_outliers <-
    mydataframe %>% 
    group_by_(myparams[1]) %>% 
    summarise(rt_participant_mean = mean(rt, na.rm = T),
              rt_participant_sd = sd(rt, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(rt_grand_mean = mean(rt_participant_mean, na.rm = T),
           rt_grand_sd = sd(rt_participant_mean, na.rm = T))

  rt_trial_outliers <-
    mydataframe %>% 
    ungroup() %>% 
    group_by(...) %>% 
    summarise(rt_conditional_mean = mean(rt, na.rm = T),
              rt_conditional_sd = sd(rt, na.rm = T))
  print("Printing conditional means dataframe...")
  print(rt_trial_outliers)
  mydataframe <-
    mydataframe %>% 
    left_join(., rt_participant_outliers, by = myparams[1])
  mydataframe <-
    mydataframe %>% 
    left_join(., rt_trial_outliers, by = myparams)



mydataframe <-
  mydataframe %>% 
  group_by(participant_id) %>% 
  mutate(drop_trial_rt = case_when(rt_conditional_mean + trial_sd_cut * rt_conditional_sd < rt ~ 1L,
                                   rt_conditional_mean - trial_sd_cut * rt_conditional_sd > rt ~ 1L,
                                   TRUE ~ 0L),
         drop_participant_rt = case_when(rt_grand_mean + participant_sd_cut * rt_grand_sd < rt_participant_mean ~ 1L,
                                         rt_grand_mean - participant_sd_cut * rt_grand_sd > rt_participant_mean ~ 1L,
                                         TRUE ~ 0L))


}

# This separate function is used to exclude participants and trials on variables added in the 
# previous function.
# Use drop_prev_errors=T and prev_correct if you have previous conflict trials in the 
# conditions and you want to exlude erroneous previous trials.

# WIP, need standalone variables
global_exclusion = function(data, drop_prev_errors = F){
    data %>% 
    mutate(age_drop = case_when(age < 18 ~ 1L,
                                age > 99 ~ 1L,
                                TRUE ~ 0L)) %>% 
    filter(age_drop != 1L) %>% 
      filter(drop_participant_acc == 0L,
             drop_participant_rt  == 0L) %>% 
      filter(is_correct            == 1L,   # 1 exclusion criteria
             drop_trial_rt         == 0L) %>% # 2 exclusion criteria 
      {if(drop_prev_errors) filter(.,is_nmin3_correct == 1L) else .} # 3 conditional exclusion criteria
  
}
