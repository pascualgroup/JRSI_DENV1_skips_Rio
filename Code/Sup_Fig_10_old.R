rm(list = ls())
source("load_libraries_essential.R")
library(zoo)
library(pomp)
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
Sup_Fig_10A_df_colnames = c("gamma","LL", "Model",     
                           "Model_Name", "Profile_Var")
Sup_Fig_10A_prof_peak_treshold_df_colnames = c("Profile_threshold", "Model", "Model_Name")
Sup_Fig_10A_MLE_value_for_prof_var_df_colnames = c("MLE_value_for_prof_var", "Model", "Model_Name")
Sup_Fig_10A_prof_peak_value_for_prof_var_df_colnames = c("prof_peak_value_for_prof_var", "Model", "Model_Name")


ML_df_colnames = c("ML", "Model", "Model_Name")


Sup_Fig_10A_df = data.frame(matrix(nrow = 0,
                                  ncol = length(Sup_Fig_10A_df_colnames)))
Sup_Fig_10A_prof_peak_treshold_df =
  data.frame(matrix(nrow = 0,ncol = length(Sup_Fig_10A_prof_peak_treshold_df_colnames)))
Sup_Fig_10A_MLE_value_for_prof_var_df =
  data.frame(matrix(nrow = 0, ncol = length(Sup_Fig_10A_MLE_value_for_prof_var_df_colnames)))
Sup_Fig_10A_prof_peak_value_for_prof_var_df =
  data.frame(matrix(nrow = 0, ncol = length(Sup_Fig_10A_prof_peak_value_for_prof_var_df_colnames)))


ML_df = data.frame(matrix(nrow = 0,
                                  ncol = length(ML_df_colnames)))

colnames(Sup_Fig_10A_df) = Sup_Fig_10A_df_colnames
colnames(Sup_Fig_10A_prof_peak_treshold_df) = Sup_Fig_10A_prof_peak_treshold_df_colnames
colnames(Sup_Fig_10A_MLE_value_for_prof_var_df) = Sup_Fig_10A_MLE_value_for_prof_var_df_colnames
colnames(Sup_Fig_10A_prof_peak_value_for_prof_var_df) = Sup_Fig_10A_prof_peak_value_for_prof_var_df_colnames


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
  #Get data for Sup Figure 10A
  profile_var = "gamma"
  single_model_output_list = get_profile_df(profile_var = profile_var, model_name = model_name,
                                            model_label = model_label, MLE = MLE)
  Sup_Fig_10A_df = rbind(Sup_Fig_10A_df, single_model_output_list[[1]])
  Sup_Fig_10A_prof_peak_treshold_df = rbind(Sup_Fig_10A_prof_peak_treshold_df, single_model_output_list[[2]])
  Sup_Fig_10A_MLE_value_for_prof_var_df = rbind(Sup_Fig_10A_MLE_value_for_prof_var_df,
                                               single_model_output_list[[3]])
  Sup_Fig_10A_prof_peak_value_for_prof_var_df = rbind(Sup_Fig_10A_prof_peak_value_for_prof_var_df,
                                               single_model_output_list[[4]])
  
  
}

##Plotting function
# profile_var = "sigma_P"
# Sup_Fig_df = Sup_Fig_10A_df
# Sup_Fig_prof_peak_treshold_df = Sup_Fig_10A_prof_peak_treshold_df
# Fig_lab = "10A"
Sup_Fig_10_A = plot_profiles(profile_var = "gamma", Sup_Fig_df = Sup_Fig_10A_df,
                            Sup_Fig_prof_peak_treshold_df = Sup_Fig_10A_prof_peak_treshold_df,
                            Sup_Fig_MLE_value_for_prof_var_df = Sup_Fig_10A_MLE_value_for_prof_var_df,
                            Sup_Fig_prof_peak_value_for_prof_var_df =
                              Sup_Fig_10A_prof_peak_value_for_prof_var_df,
                            Fig_lab = "10A", ML_df = ML_df)

profile_var = "gamma"
#Load results
profile_data = read.csv(file = paste0("../Generated_Data/Profiles/", model_name, "_Model/", profile_var, "_Profile/",
                                      profile_var, "_", model_name, "_profile_combined_data.csv"))
#head(profile_data)

na_data = filter(profile_data, is.na(LL) == TRUE)
print(paste("There are ", nrow(na_data), " entries with NA likelihoods"))

profile_data_clean = na.omit(profile_data)


ML = max(profile_data_clean$LL)
cutoff_thres_20_LL_from_ML = ML - 20



cutoff_thres_2_LL_from_ML = ML - 2




### Take trace of profile (max at each value of profile variable)
profile_var_profile = aggregate(formula(paste0("LL ~ ",eval(profile_var))), profile_data_clean, max)
gamma_profile_all_params =join(profile_var_profile, profile_data_clean)

Sup_Fig_10_B = ggplot(data = gamma_profile_all_params,
                      aes(x = gamma, y = rho,
                          shape = LL > Sup_Fig_10A_prof_peak_treshold_df$Profile_threshold ),
                      size = 3 ) +
    geom_point() + rahul_theme +
  scale_shape_manual(values = c(1,16)) + theme(legend.position = "None")
  Sup_Fig_10_B
pdf("../Figures/Supplemental_Figures/Supplemental_Figure_10/Sup_Figure_10B.pdf")
print(Sup_Fig_10_B)
dev.off()

gamma_profile_all_params$R_naught = gamma_profile_all_params$Beta_0/(gamma_profile_all_params$gamma + gamma_profile_all_params$mu_H)
range(gamma_profile_all_params$R_naught)

Sup_Fig_10_C = ggplot(data = gamma_profile_all_params,
                      aes(x = gamma, y = R_naught,
                          shape = LL > Sup_Fig_10A_prof_peak_treshold_df$Profile_threshold  )) +
  geom_point() + rahul_theme +scale_shape_manual(values = c(1,16)) +
  theme(legend.position = "None")
Sup_Fig_10_C
pdf("../Figures/Supplemental_Figures/Supplemental_Figure_10/Sup_Figure_10C.pdf")
print(Sup_Fig_10_C)
dev.off()
combined_data = gamma_profile_all_params




combined_data$LL_shape = combined_data$LL > Sup_Fig_10A_prof_peak_treshold_df$Profile_threshold
combined_data = combined_data %>%
  mutate("R[0]" = R_naught)
combined_data_melt = melt(combined_data, id.vars = c("gamma", "LL_shape", "phi",
                                                       "sigma_P", "sigma_M", "Beta_0",
                                                       "delta", "mu_H", "N_0", "I_0", "R_0",
                                                       "C_0", "r", "omega", "epsilon", "seed", "LL",
                                                     "R_naught"))

head(combined_data_melt)
S10_plot = ggplot(data = combined_data_melt, aes(x = gamma, y = value, shape = LL_shape)) +
  geom_point(size = 3) + facet_wrap(~variable, scales = "free",
                                    strip.position = "left", labeller=label_parsed,
                                    ncol = 1) +
  rahul_man_figure_theme +theme_white_background  +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    strip.placement = "outside"
  ) +
  theme(axis.title.y=element_blank()) +
  theme(legend.position = "None") +
  xlab(expression(paste("Recovery Rate ", (gamma)))) +
  scale_shape_manual(values = c(1,16))
S10_plot

# Combined Plot -----------------------------------------------------------


pdf("../Figures/Supplemental_Figures/Supplemental_Figure_10/Supplemental_Figure_10_Combined.pdf",
    height = 10, width = 5)

print(S10_plot)
dev.off()



