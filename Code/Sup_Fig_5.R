rm(list = ls())
source("load_libraries_essential.R")
library(zoo)
library(pomp2)
source("rahul_theme.R")
source("Sup_Figure_profile_facet_plots_plot_functions.R")
rahul_poster_theme = theme(
  axis.title.x = element_text(size = 23,
                              face = "bold",
                              color = "black"),
  axis.text.x = element_text(size = 21,
                             face = "bold",
                             color = "black"),
  axis.title.y = element_text(size = 23,
                              face = "bold",
                              color = "black"),
  legend.title = element_text(size = 21,
                              face = "bold",
                              color = "black"),
  legend.text = element_text(size = 23,
                             face = "bold",
                             color = "black"),
  axis.text.y = element_text(size = 21,
                             face = "bold",
                             color = "black")
)
model_name_list = c("A_7")
model_label_list = factor(c("SIR Cosine No Immigration"))
model_label_list = factor(model_label_list, levels = c("SIR Cosine No Immigration"))

Csnippet_file_path_list = c("Csnippet_SIR_cosine_model.R")
Num_est_parameters_list = c(7)
data_file_path_list = c("../Generated_Data/Rio_DENV1_Data_2_25_years_clean.csv")

num_years_list = c(2.50)

model_ref_df = data.frame(model_name = model_name_list, model_label = model_label_list,
                          Csnippet_file_path = Csnippet_file_path_list,
                          Num_est_parameters = Num_est_parameters_list,
                          data_file_path = data_file_path_list,
                          num_years = num_years_list,stringsAsFactors = FALSE)
Sup_Fig_4A_df_colnames = c("Beta_0","LL", "Model",     
                           "Model_Name", "Profile_Var")
Sup_Fig_4A_prof_peak_treshold_df_colnames = c("Profile_threshold", "Model", "Model_Name")
Sup_Fig_4A_MLE_value_for_prof_var_df_colnames = c("MLE_value_for_prof_var", "Model", "Model_Name")
Sup_Fig_4A_prof_peak_value_for_prof_var_df_colnames = c("prof_peak_value_for_prof_var", "Model", "Model_Name")


Sup_Fig_4B_df_colnames = c("phi","LL" ,"Model",     
                           "Model_Name", "Profile_Var")
Sup_Fig_4B_prof_peak_treshold_df_colnames = c("Profile_threshold", "Model", "Model_Name")
Sup_Fig_4B_MLE_value_for_prof_var_df_colnames = c("MLE_value_for_prof_var", "Model", "Model_Name")
Sup_Fig_4B_prof_peak_value_for_prof_var_df_colnames = c("prof_peak_value_for_prof_var", "Model", "Model_Name")

Sup_Fig_4C_df_colnames = c("delta","LL" ,"Model",     
                           "Model_Name", "Profile_Var")
Sup_Fig_4C_prof_peak_treshold_df_colnames = c("Profile_threshold", "Model", "Model_Name")
Sup_Fig_4C_MLE_value_for_prof_var_df_colnames = c("MLE_value_for_prof_var", "Model", "Model_Name")
Sup_Fig_4C_prof_peak_value_for_prof_var_df_colnames = c("prof_peak_value_for_prof_var", "Model", "Model_Name")

ML_df_colnames = c("ML", "Model", "Model_Name")


Sup_Fig_4A_df = data.frame(matrix(nrow = 0,
                                  ncol = length(Sup_Fig_4A_df_colnames)))


Sup_Fig_4A_prof_peak_treshold_df = data.frame(matrix(nrow = 0,
                                                     ncol = length(Sup_Fig_4A_prof_peak_treshold_df_colnames)))
Sup_Fig_4A_MLE_value_for_prof_var_df =
  data.frame(matrix(nrow = 0, ncol = length(Sup_Fig_4A_MLE_value_for_prof_var_df_colnames)))
Sup_Fig_4A_prof_peak_value_for_prof_var_df =
  data.frame(matrix(nrow = 0, ncol = length(Sup_Fig_4A_prof_peak_value_for_prof_var_df_colnames)))

Sup_Fig_4B_df = data.frame(matrix(nrow = 0,
                                  ncol = length(Sup_Fig_4B_df_colnames)))
Sup_Fig_4B_prof_peak_treshold_df = data.frame(matrix(nrow = 0,
                                                     ncol = length(Sup_Fig_4B_prof_peak_treshold_df_colnames)))
Sup_Fig_4B_MLE_value_for_prof_var_df =
  data.frame(matrix(nrow = 0, ncol = length(Sup_Fig_4B_MLE_value_for_prof_var_df_colnames)))
Sup_Fig_4B_prof_peak_value_for_prof_var_df =
  data.frame(matrix(nrow = 0, ncol = length(Sup_Fig_4B_prof_peak_value_for_prof_var_df_colnames)))

Sup_Fig_4C_df = data.frame(matrix(nrow = 0,
                                  ncol = length(Sup_Fig_4C_df_colnames)))
Sup_Fig_4C_prof_peak_treshold_df = data.frame(matrix(nrow = 0,
                                                     ncol = length(Sup_Fig_4C_prof_peak_treshold_df_colnames)))
Sup_Fig_4C_MLE_value_for_prof_var_df =
  data.frame(matrix(nrow = 0, ncol = length(Sup_Fig_4C_MLE_value_for_prof_var_df_colnames)))
Sup_Fig_4C_prof_peak_value_for_prof_var_df =
  data.frame(matrix(nrow = 0, ncol = length(Sup_Fig_4C_prof_peak_value_for_prof_var_df_colnames)))

ML_df = data.frame(matrix(nrow = 0,
                          ncol = length(ML_df_colnames)))

colnames(Sup_Fig_4A_df) = Sup_Fig_4A_df_colnames
colnames(Sup_Fig_4A_prof_peak_treshold_df) = Sup_Fig_4A_prof_peak_treshold_df_colnames
colnames(Sup_Fig_4A_MLE_value_for_prof_var_df) = Sup_Fig_4A_MLE_value_for_prof_var_df_colnames
colnames(Sup_Fig_4A_prof_peak_value_for_prof_var_df) = Sup_Fig_4A_prof_peak_value_for_prof_var_df_colnames


colnames(Sup_Fig_4B_df) = Sup_Fig_4B_df_colnames
colnames(Sup_Fig_4B_prof_peak_treshold_df) = Sup_Fig_4B_prof_peak_treshold_df_colnames
colnames(Sup_Fig_4B_MLE_value_for_prof_var_df) = Sup_Fig_4B_MLE_value_for_prof_var_df_colnames
colnames(Sup_Fig_4B_prof_peak_value_for_prof_var_df) = Sup_Fig_4B_prof_peak_value_for_prof_var_df_colnames

colnames(Sup_Fig_4C_df) = Sup_Fig_4C_df_colnames
colnames(Sup_Fig_4C_prof_peak_treshold_df) = Sup_Fig_4C_prof_peak_treshold_df_colnames
colnames(Sup_Fig_4C_MLE_value_for_prof_var_df) = Sup_Fig_4C_MLE_value_for_prof_var_df_colnames
colnames(Sup_Fig_4C_prof_peak_value_for_prof_var_df) = Sup_Fig_4C_prof_peak_value_for_prof_var_df_colnames

colnames(ML_df) = ML_df_colnames

for(model_index in seq(1:length(model_name_list))){
  print(model_index)
  model_name = as.character(model_name_list[model_index])
  single_model_ref_data = filter(model_ref_df, model_name == !!model_name)
  model_label = single_model_ref_data$model_label
  Csnippet_file_path = single_model_ref_data$Csnippet_file_path
  Num_est_parameters = single_model_ref_data$Num_est_parameters
  data_file_path = single_model_ref_data$data_file_path
  num_years = single_model_ref_data$num_years
  
  Rio_data_clean = read.csv(file = data_file_path)
  
  Rio_clean_data = Rio_data_clean
  #head(Rio_data_clean)
  
  source(Csnippet_file_path, local = TRUE)
  
  #Set t0
  t0 = as.numeric(as.Date("1986/05/01") - as.Date("1986/01/01"))
  
  #Load param combination directory
  combined_profile_data = read.csv(file = paste0("../Generated_Data/Profiles/", model_name,
                                                 "_Model/combined_", model_name,"_profile_data_directory.csv"))
  
  
  #head(combined_profile_data)
  ML = max(combined_profile_data$LL, na.rm = TRUE)
  MLE = filter(combined_profile_data, LL >= ML)
  
  ML_params = dplyr::select(MLE, -one_of("seed", "LL", "Profile_Type"))
  
  MLE
  single_model_ML_df = data.frame(ML = ML, Model = model_name, Model_Name = model_label)
  ML_df = rbind(ML_df, single_model_ML_df)
  #Get data for Sup Figure 4A
  profile_var = "Beta_0"
  single_model_output_list = get_profile_df(profile_var = profile_var, model_name = model_name,
                                            model_label = model_label, MLE = MLE)
  Sup_Fig_4A_df = rbind(Sup_Fig_4A_df, single_model_output_list[[1]])
  Sup_Fig_4A_prof_peak_treshold_df = rbind(Sup_Fig_4A_prof_peak_treshold_df, single_model_output_list[[2]])
  Sup_Fig_4A_MLE_value_for_prof_var_df = rbind(Sup_Fig_4A_MLE_value_for_prof_var_df,
                                               single_model_output_list[[3]])
  Sup_Fig_4A_prof_peak_value_for_prof_var_df = rbind(Sup_Fig_4A_prof_peak_value_for_prof_var_df,
                                                     single_model_output_list[[4]])
  
  #Get data for Sup Figure 4B
  profile_var = "phi"
  single_model_output_list = get_profile_df(profile_var = profile_var, model_name = model_name,
                                            model_label = model_label, MLE = MLE)
  Sup_Fig_4B_df = rbind(Sup_Fig_4B_df, single_model_output_list[[1]])
  Sup_Fig_4B_prof_peak_treshold_df = rbind(Sup_Fig_4B_prof_peak_treshold_df, single_model_output_list[[2]])
  Sup_Fig_4B_MLE_value_for_prof_var_df = rbind(Sup_Fig_4B_MLE_value_for_prof_var_df,
                                               single_model_output_list[[3]])
  Sup_Fig_4B_prof_peak_value_for_prof_var_df = rbind(Sup_Fig_4B_prof_peak_value_for_prof_var_df,
                                                     single_model_output_list[[4]])
  
  
  #Get data for Sup Figure 4C
  profile_var = "delta"
  single_model_output_list = get_profile_df(profile_var = profile_var, model_name = model_name,
                                            model_label = model_label, MLE = MLE)
  Sup_Fig_4C_df = rbind(Sup_Fig_4C_df, single_model_output_list[[1]])
  Sup_Fig_4C_prof_peak_treshold_df = rbind(Sup_Fig_4C_prof_peak_treshold_df, single_model_output_list[[2]])
  Sup_Fig_4C_MLE_value_for_prof_var_df = rbind(Sup_Fig_4C_MLE_value_for_prof_var_df,
                                               single_model_output_list[[3]])
  Sup_Fig_4C_prof_peak_value_for_prof_var_df = rbind(Sup_Fig_4C_prof_peak_value_for_prof_var_df,
                                                     single_model_output_list[[4]])
  
}

##Plotting function

Sup_Fig_4_A = plot_profiles(profile_var = "Beta_0", Sup_Fig_df = Sup_Fig_4A_df,
                            Sup_Fig_prof_peak_treshold_df = Sup_Fig_4A_prof_peak_treshold_df,
                            Sup_Fig_MLE_value_for_prof_var_df = Sup_Fig_4A_MLE_value_for_prof_var_df,
                            Sup_Fig_prof_peak_value_for_prof_var_df =
                              Sup_Fig_4A_prof_peak_value_for_prof_var_df,
                            Fig_lab = "4A", ML_df = ML_df)
Sup_Fig_4_B = plot_profiles(profile_var = "phi", Sup_Fig_df = Sup_Fig_4B_df,
                            Sup_Fig_prof_peak_treshold_df = Sup_Fig_4B_prof_peak_treshold_df,
                            Sup_Fig_MLE_value_for_prof_var_df = Sup_Fig_4B_MLE_value_for_prof_var_df,
                            Sup_Fig_prof_peak_value_for_prof_var_df =
                              Sup_Fig_4B_prof_peak_value_for_prof_var_df,
                            Fig_lab = "4B", ML_df = ML_df)
Sup_Fig_4_C = plot_profiles(profile_var = "delta", Sup_Fig_df = Sup_Fig_4C_df,
                            Sup_Fig_prof_peak_treshold_df = Sup_Fig_4C_prof_peak_treshold_df,
                            Sup_Fig_MLE_value_for_prof_var_df = Sup_Fig_4C_MLE_value_for_prof_var_df,
                            Sup_Fig_prof_peak_value_for_prof_var_df =
                              Sup_Fig_4C_prof_peak_value_for_prof_var_df,
                            Fig_lab = "4C", ML_df = ML_df)

# Combined Plot -----------------------------------------------------------

library(gridExtra)
library(grid)
library(lattice)

rahul_panel_theme = theme(
  axis.title.x = element_text(size = 10,
                              face = "bold",
                              color = "black"),
  axis.text.x = element_text(size = 10,
                             face = "bold",
                             color = "black"),
  axis.title.y = element_text(size = 10,
                              face = "bold",
                              color = "black"),
  legend.title = element_text(size = 10,
                              face = "bold",
                              color = "black"),
  legend.text = element_text(size = 10,
                             face = "bold",
                             color = "black"),
  axis.text.y = element_text(size = 8,
                             face = "bold",
                             color = "black")
)

Sup_Fig_4_A_comb = Sup_Fig_4_A + rahul_panel_theme + theme(legend.position = "None")
Sup_Fig_4_B_comb = Sup_Fig_4_B + rahul_panel_theme + theme(legend.position = "None")
Sup_Fig_4_C_comb = Sup_Fig_4_C + rahul_panel_theme + theme(legend.position = "None")



legend = load(file = "../Generated_Data/Data_for_Supplemental_Figures/Sup_Fig_3_A_small_legend.Rdata")
legend <- cowplot::get_legend(Sup_Fig_3_A_small_legend)

pdf("../Figures/Supplemental_Figures/Supplemental_Figure_4/Supplemental_Figure_4_Combined.pdf")
# print(grid.arrange(Sup_Fig_3_A_comb, Sup_Fig_3_B_comb, legend,Sup_Fig_3_C_comb, Sup_Fig_3_D_comb,
#                    ncol = 3, widths = c(1,1,0.9)))
print(grid.arrange(Sup_Fig_4_A_comb, Sup_Fig_4_B_comb, Sup_Fig_4_C_comb,legend,
                   ncol = 2, widths = c(1,1)))
dev.off()