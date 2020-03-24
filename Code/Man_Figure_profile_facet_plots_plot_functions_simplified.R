
## Function to get profile data for plotting (Supplemental Figure 3 Plot Code)
get_profile_df = function(profile_var, model_name, model_label, MLE){
  #Load results
  profile_data = read.csv(file = paste0("../Generated_Data/Profiles/", model_name, "_Model/", profile_var, "_Profile/",
                                        profile_var, "_", model_name, "_profile_combined_data.csv"))
  #head(profile_data)
  
  na_data = filter(profile_data, is.na(LL) == TRUE)
  print(paste("There are ", nrow(na_data), " entries with NA likelihoods"))
  
  profile_data_clean = na.omit(profile_data)
  
  
  profile_var_profile = aggregate(formula(paste0("LL ~ ",eval(profile_var))), profile_data_clean, max)
  profile_all_params =profile_var_profile
  MLE_prof_threshold  = MLE$LL - 2
  prof_peak_threshold = max(profile_all_params$LL) - 2

  profile_all_params$Model = model_name
  profile_all_params$Model_Name = model_label
  profile_all_params$Profile_Var = profile_var
  
  single_model_prof_peak_treshold_df = data.frame(Profile_threshold = prof_peak_threshold,
                                                  Model = model_name,
                                                  Model_Name = model_label)
  MLE_value_for_prof_var = dplyr::select(MLE, eval(profile_var))
  MLE_value_for_prof_var_df = data.frame(MLE_value_for_prof_var = as.numeric(MLE_value_for_prof_var),
                                                  Model = model_name,
                                                  Model_Name = model_label)
  prof_peak_value_for_prof_var = dplyr::select(filter(profile_all_params, LL == max(LL)),
                                               eval(profile_var) )
  prof_peak_value_for_prof_var_df = data.frame(prof_peak_value_for_prof_var =
                                                 as.numeric(prof_peak_value_for_prof_var),
                                         Model = model_name,
                                         Model_Name = model_label)
  output_list = list(profile_all_params, single_model_prof_peak_treshold_df, MLE_value_for_prof_var_df,
                     prof_peak_value_for_prof_var_df)
  
}


## Function to get slice data for plotting (Supplemental Figure 8 Plot Code)
get_slice_df = function(profile_var, model_name, model_label, MLE, slice_var){
  #Load results
  profile_data = read.csv(file = paste0("../Generated_Data/Profiles/", model_name, "_Model/", profile_var, "_Profile/",
                                        profile_var, "_", model_name, "_profile_combined_data.csv"))
  #head(profile_data)
  
  na_data = filter(profile_data, is.na(LL) == TRUE)
  print(paste("There are ", nrow(na_data), " entries with NA likelihoods"))
  
  profile_data_clean = na.omit(profile_data)
  
  
  slice_var_profile = aggregate(formula(paste0("LL ~ ",eval(slice_var))), profile_data_clean, max)
  profile_all_params =slice_var_profile
  MLE_prof_threshold  = MLE$LL - 2
  prof_peak_threshold = max(profile_all_params$LL) - 2
  
  profile_all_params$Model = model_name
  profile_all_params$Model_Name = model_label
  profile_all_params$slice_var = slice_var
  
  single_model_prof_peak_treshold_df = data.frame(Profile_threshold = prof_peak_threshold,
                                                  Model = model_name,
                                                  Model_Name = model_label)
  MLE_value_for_prof_var = dplyr::select(MLE, eval(slice_var))
  MLE_value_for_prof_var_df = data.frame(MLE_value_for_prof_var = as.numeric(MLE_value_for_prof_var),
                                         Model = model_name,
                                         Model_Name = model_label)
  prof_peak_value_for_prof_var = dplyr::select(filter(profile_all_params, LL == max(LL)),
                                               eval(slice_var) )
  prof_peak_value_for_prof_var_df = data.frame(prof_peak_value_for_prof_var =
                                                 as.numeric(prof_peak_value_for_prof_var),
                                               Model = model_name,
                                               Model_Name = model_label)
  output_list = list(profile_all_params, single_model_prof_peak_treshold_df, MLE_value_for_prof_var_df,
                     prof_peak_value_for_prof_var_df)
  
}

## Function to plot profile data (Supplemental Figure 3 Plot Code)
plot_profiles_simple = function(Sup_Fig_df, Sup_Fig_prof_peak_treshold_df,
                         Sup_Fig_MLE_value_for_prof_var_df,
                         Sup_Fig_prof_peak_value_for_prof_var_df,
                         profile_var, Fig_lab, ML_df, ncol_facet = 1,
                         comb_plot = FALSE,
                         plot_var_label = NULL){
  Main_fig_num = substr(Fig_lab,1,1)
  ML_df$Line_Col_Group = as.factor(
    as.numeric(ML_df$ML-2 == Sup_Fig_prof_peak_treshold_df$Profile_threshold)*4 +1)
  Sup_Fig_prof_peak_treshold_df$Line_Col_Group = as.factor(
    as.numeric(ML_df$ML-2 == Sup_Fig_prof_peak_treshold_df$Profile_threshold)*3 +2)
  
  ML_df$Line_Type_Group = as.factor(
    as.numeric(ML_df$ML-2 == Sup_Fig_prof_peak_treshold_df$Profile_threshold)*2 +1)
  Sup_Fig_prof_peak_treshold_df$Line_Type_Group = as.factor(
    as.numeric(ML_df$ML-2 == Sup_Fig_prof_peak_treshold_df$Profile_threshold)*1 +2)
  
  Sup_Fig_MLE_value_for_prof_var_df$Line_Col_Group = as.factor(
     (as.numeric(Sup_Fig_MLE_value_for_prof_var_df$MLE_value_for_prof_var ==
                 Sup_Fig_prof_peak_value_for_prof_var_df$prof_peak_value_for_prof_var)*3 +3))
  Sup_Fig_prof_peak_value_for_prof_var_df$Line_Col_Group = as.factor(
     (as.numeric(Sup_Fig_MLE_value_for_prof_var_df$MLE_value_for_prof_var ==
                 Sup_Fig_prof_peak_value_for_prof_var_df$prof_peak_value_for_prof_var)*2 +4))
  
  Sup_Fig_MLE_value_for_prof_var_df$Line_Type_Group = as.factor(
    0 + (as.numeric(Sup_Fig_MLE_value_for_prof_var_df$MLE_value_for_prof_var ==
                      Sup_Fig_prof_peak_value_for_prof_var_df$prof_peak_value_for_prof_var)*2 +1))
  Sup_Fig_prof_peak_value_for_prof_var_df$Line_Type_Group = as.factor(
    0 + (as.numeric(Sup_Fig_MLE_value_for_prof_var_df$MLE_value_for_prof_var ==
                      Sup_Fig_prof_peak_value_for_prof_var_df$prof_peak_value_for_prof_var) +2))
  
  
  ymin = min(Sup_Fig_prof_peak_treshold_df$Profile_threshold)-10
  y_thres = max(ymin, min(Sup_Fig_df$LL))
  y_thres = min(y_thres,min(Sup_Fig_prof_peak_treshold_df$Profile_threshold) )
  p = ggplot() +
    geom_point(data = Sup_Fig_df, aes_string(x = eval(profile_var), y = "LL"))  + rahul_theme +
    geom_hline(data = ML_df, aes(yintercept = ML -2,
                                 color =  Line_Col_Group), size = 1.0) +
    geom_vline(data = Sup_Fig_MLE_value_for_prof_var_df,
               aes(xintercept = MLE_value_for_prof_var,
                   color =  Line_Col_Group), size = 1.0, show.legend= F) 
  
   

    if(length(unique(Sup_Fig_prof_peak_value_for_prof_var_df$Line_Type_Group)) == 1 && unique(Sup_Fig_prof_peak_value_for_prof_var_df$Line_Type_Group) == c("3")){
    p = p + scale_color_manual(name="",values=c( "purple", "pink"),
                               breaks = c( "5", "6"), labels = c("2 LL from MLE and Profile Peak",
                                                                                   "MLE and Profile Peak Value")) 
  }else{
    p = p + scale_color_manual(name="",values=c("red", "blue",  "darkgreen", "orange", "purple", "pink"),
                               breaks = c("1","2", "3", "4", "5", "6"), labels = c("2 LL from MLE",
                                                                                   "2 LL from Profile Peak",
                                                                                   "MLE Profile Value",
                                                                                   "Profile Peak Profile Value",
                                                                                   "2 LL from MLE and Profile Peak",
                                                                                   "MLE and Profile Peak Value")) 
  }
    
    
  
  p
  rahul_panel_theme = theme(
    axis.title.x = element_text(size = 10,
                                face = "bold",
                                color = "black"),
    axis.text.x = element_text(size = 10,
                               face = "bold",
                               color = "black"),
    axis.title.y = element_text(size = 8,
                                face = "bold",
                                color = "black"),
    legend.title = element_text(size = 8,
                                face = "bold",
                                color = "black"),
    legend.text = element_text(size = 8,
                               face = "bold",
                               color = "black"),
    axis.text.y = element_text(size = 8,
                               face = "bold",
                               color = "black")
  )
  if(comb_plot == TRUE){
    p = p + rahul_panel_theme +theme_white_background
  }else{
    p = p + theme_white_background
  }
  # pdf(paste0("../Figures/Supplemental_Figures/Supplemental_Figure_", Main_fig_num,
  #            "/Supplemental_Figure_",Fig_lab,".pdf"))
  # print(p)
  # dev.off()
  
  
  
  p = p +xlab(parse(text = paste0(plot_var_label)))
  
  return(p)
}

## Function to plot slice data (Supplemental Figure 8 Plot Code)
plot_slices = function(Sup_Fig_df, 
                         Sup_Fig_MLE_value_for_prof_var_df,
                         slice_var, Fig_lab, ML_df, ncol_facet = 1,
                         comb_plot = FALSE){
  Main_fig_num = substr(Fig_lab,1,1)
  
  ML_df$Line_Col_Group = as.factor(1)
  ML_df$Line_Type_Group = as.factor(1)
  
  Sup_Fig_MLE_value_for_prof_var_df$Line_Col_Group = as.factor(2)
  Sup_Fig_MLE_value_for_prof_var_df$Line_Type_Group = as.factor(1)
  ymin = max(Sup_Fig_df$LL)-20
  y_thres = max(ymin, min(Sup_Fig_df$LL))
  p = ggplot() +
    geom_point(data = Sup_Fig_df, aes_string(x = eval(slice_var), y = "LL"))  + rahul_theme +
    geom_hline(data = ML_df, aes(yintercept = ML -2,
                                 color =  Line_Col_Group,
                                 linetype = Line_Type_Group), size = 1.0) +
    geom_vline(data = Sup_Fig_MLE_value_for_prof_var_df,
               aes(xintercept = MLE_value_for_prof_var,
                   color =  Line_Col_Group,
                   linetype = Line_Type_Group), size = 1.0, show.legend= F) +
    ylim(y_thres, NA) 
  p = p + scale_color_manual(name="",values=c("red", "darkgreen"),
                             breaks = c("1","2"), labels = c("2 LL from MLE","MLE Profile Value")) +
    scale_linetype_manual(name="Line Type Legend",values=c(4), labels = c("MLE"))
  
  
  
  
  p
  rahul_panel_theme = theme(
    axis.title.x = element_text(size = 10,
                                face = "bold",
                                color = "black"),
    axis.text.x = element_text(size = 10,
                               face = "bold",
                               color = "black"),
    axis.title.y = element_text(size = 8,
                                face = "bold",
                                color = "black"),
    legend.title = element_text(size = 8,
                                face = "bold",
                                color = "black"),
    legend.text = element_text(size = 8,
                               face = "bold",
                               color = "black"),
    axis.text.y = element_text(size = 8,
                               face = "bold",
                               color = "black")
  )
  if(comb_plot == TRUE){
    p = p + rahul_panel_theme + facet_wrap(~Model_Name, ncol = ncol_facet, scales = "free") +
      theme_white_background
  }else{
    p = p + facet_wrap(~Model_Name, ncol = ncol_facet)
  }
  pdf(paste0("../Figures/Supplemental_Figures/Supplemental_Figure_", Main_fig_num,
             "/Supplemental_Figure_",Fig_lab,".pdf"))
  print(p)
  dev.off()
  
  return(p)
}

