# ---- plot_functions_simplified ----
## Function to get profile data
## for plotting ( Figure 3 Plot Code)
get_profile_df = function(profile_var,
                          model_name, model_label, MLE){
  #Load results
  profile_data = read.csv(
    file = paste0("../Generated_Data/Profiles/",
                  model_name, "_Model/",
                  profile_var, "_Profile/",
                  profile_var, "_",
                  model_name, "_profile_combined_data.csv"))

  na_data = filter(profile_data,
                   is.na(LL) == TRUE)
  print(paste("There are ", nrow(na_data),
              " entries with NA likelihoods"))
  
  profile_data_clean = na.omit(profile_data)
  
  
  profile_var_profile = aggregate(
    formula(paste0("LL ~ ",eval(profile_var))),
    profile_data_clean, max)
  profile_all_params =profile_var_profile
  MLE_prof_threshold  = MLE$LL - 2
  prof_peak_threshold = max(profile_all_params$LL) - 2

  profile_all_params$Model = model_name
  profile_all_params$Model_Name = model_label
  profile_all_params$Profile_Var = profile_var
  
  single_model_prof_peak_treshold_df =
    data.frame(Profile_threshold = prof_peak_threshold,
               Model = model_name,
               Model_Name = model_label)
  MLE_value_for_prof_var = dplyr::select(MLE,
                                         eval(profile_var))
  MLE_value_for_prof_var_df = data.frame(
    MLE_value_for_prof_var = as.numeric(MLE_value_for_prof_var),
    Model = model_name, Model_Name = model_label)
  prof_peak_value_for_prof_var = dplyr::select(
    filter(profile_all_params, LL == max(LL)),
    eval(profile_var) )
  prof_peak_value_for_prof_var_df = data.frame(
    prof_peak_value_for_prof_var = as.numeric(
      prof_peak_value_for_prof_var),
    Model = model_name, Model_Name = model_label)
  output_list = list(profile_all_params,
                     single_model_prof_peak_treshold_df,
                     MLE_value_for_prof_var_df,
                     prof_peak_value_for_prof_var_df)
  
}




