rm(list = ls())
source("load_libraries_essential.R")
library(zoo)
library(pomp)
source("rahul_theme.R")
source("Man_Figure_profile_facet_plots_plot_functions_simplified.R")
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
model_name_list = c("A_7", "A_6",
                    "A_5" )
model_label_list = factor(c("SIR Cosine", "SIR Spline",
                            "SEIR Spline"))
model_label_list = factor(model_label_list, levels = c("SIR Cosine", "SIR Spline",
                                                       "SEIR Spline"))

Csnippet_file_path_list = c("Csnippet_SIR_cosine_model.R",
                            "Csnippet_SIR_spline_model.R",
                            "Csnippet_SEIR_spline_model.R")
Num_est_parameters_list = c(7,7,8)
data_file_path_list = c("../Generated_Data/Rio_DENV1_Data_2_25_years_clean.csv",
                        "../Generated_Data/Rio_DENV1_Data_2_25_years_clean.csv",
                        "../Generated_Data/Rio_DENV1_Data_2_25_years_clean.csv")

num_years_list = c(2.50, 2.50, 2.50)

model_ref_df = data.frame(model_name = model_name_list, model_label = model_label_list,
                          Csnippet_file_path = Csnippet_file_path_list,
                          Num_est_parameters = Num_est_parameters_list,
                          data_file_path = data_file_path_list,
                          num_years = num_years_list,stringsAsFactors = FALSE)

Sup_Fig_5_df_colnames = c("sigma_P","LL" ,"Model",     
                           "Model_Name", "Profile_Var")
Sup_Fig_5_prof_peak_treshold_df_colnames = c("Profile_threshold", "Model", "Model_Name")
Sup_Fig_5_MLE_value_for_prof_var_df_colnames = c("MLE_value_for_prof_var", "Model", "Model_Name")
Sup_Fig_5_prof_peak_value_for_prof_var_df_colnames = c("prof_peak_value_for_prof_var", "Model", "Model_Name")


ML_df_colnames = c("ML", "Model", "Model_Name")



Sup_Fig_5_df = data.frame(matrix(nrow = 0,
                                  ncol = length(Sup_Fig_5_df_colnames)))
Sup_Fig_5_prof_peak_treshold_df = data.frame(matrix(nrow = 0,
                                                     ncol = length(Sup_Fig_5_prof_peak_treshold_df_colnames)))
Sup_Fig_5_MLE_value_for_prof_var_df =
  data.frame(matrix(nrow = 0, ncol = length(Sup_Fig_5_MLE_value_for_prof_var_df_colnames)))
Sup_Fig_5_prof_peak_value_for_prof_var_df =
  data.frame(matrix(nrow = 0, ncol = length(Sup_Fig_5_prof_peak_value_for_prof_var_df_colnames)))

ML_df = data.frame(matrix(nrow = 0,
                                  ncol = length(ML_df_colnames)))

colnames(Sup_Fig_5_df) = Sup_Fig_5_df_colnames
colnames(Sup_Fig_5_prof_peak_treshold_df) = Sup_Fig_5_prof_peak_treshold_df_colnames
colnames(Sup_Fig_5_MLE_value_for_prof_var_df) = Sup_Fig_5_MLE_value_for_prof_var_df_colnames
colnames(Sup_Fig_5_prof_peak_value_for_prof_var_df) = Sup_Fig_5_prof_peak_value_for_prof_var_df_colnames
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
  
  
  #Get data for Sup Figure 4
  profile_var = "sigma_P"
  single_model_output_list = get_profile_df(profile_var = profile_var, model_name = model_name,
                                            model_label = model_label, MLE = MLE)
  Sup_Fig_5_df = rbind(Sup_Fig_5_df, single_model_output_list[[1]])
  Sup_Fig_5_prof_peak_treshold_df = rbind(Sup_Fig_5_prof_peak_treshold_df, single_model_output_list[[2]])
  Sup_Fig_5_MLE_value_for_prof_var_df = rbind(Sup_Fig_5_MLE_value_for_prof_var_df,
                                               single_model_output_list[[3]])
  Sup_Fig_5_prof_peak_value_for_prof_var_df = rbind(Sup_Fig_5_prof_peak_value_for_prof_var_df,
                                                     single_model_output_list[[4]])
  
  
}


Sup_Fig_5_df = Sup_Fig_5_df %>%
  mutate(var_value = sigma_P) %>%
  dplyr::select(-sigma_P)

ymin = Sup_Fig_5_prof_peak_treshold_df %>%
  group_by(Model) %>%
  summarize(ymin = Profile_threshold-10) %>%
  as.data.frame() 

min_prof_value = Sup_Fig_5_df %>%
  group_by(Model) %>%
  summarize(prof_min = min(LL)) %>%
  as.data.frame()

ymin = join(ymin, min_prof_value)

y_thres_df = ymin %>%
  group_by(Model) %>%
  summarize(y_thres = max(ymin, prof_min)) %>%
  as.data.frame()

y_lim_min = min(y_thres_df$y_thres)
Sup_Fig_5_df_clean = filter(Sup_Fig_5_df, LL > y_lim_min)

plot_label_df = data.frame(Model_Name = Sup_Fig_5_MLE_value_for_prof_var_df$Model_Name,
                           plot_var_label = c("sigma_P", "sigma_P", "sigma_P"),
                           model_var_label = c("SIR Cosine", "SIR Spline", "SEIR Spline"))
Sup_Fig_5_df = join(Sup_Fig_5_df, plot_label_df)

Sup_Fig_5_ABC_plot_data = join(Sup_Fig_5_df, Sup_Fig_5_MLE_value_for_prof_var_df)


Sup_Fig_5_ABC_plot_data
head(Sup_Fig_5_ABC_plot_data)

Sup_Fig_5_ABC_plot_data = join(Sup_Fig_5_ABC_plot_data, ML_df)
cutoff_value  = -174 
Sup_Fig_5_ABC_plot_data = filter(Sup_Fig_5_ABC_plot_data, LL > ML - 10 )

Sup_Fig_5_ABC_plot_data$Metric = "LL"
Sup_Fig_5_ABC_plot_data$low_bound = ML-11
Sup_Fig_5_ABC_plot_data$Line_Color = "Show_Line"

Sup_Fig_5_combined_data = Sup_Fig_5_ABC_plot_data
Sup_Fig_5_combined_data$Model_Name = factor(Sup_Fig_5_combined_data$Model_Name, levels = c("SIR Cosine",
                                                                                           "SIR Spline","SEIR Spline"))
head(Sup_Fig_5_combined_data)

#### sigma_P Profile SIR Cosine Model
sigma_P_poly_data_SIR_Cosine = Sup_Fig_5_ABC_plot_data %>%
  filter(Profile_Var == "sigma_P") %>%
  filter(Model == "A_7") %>%
  dplyr::select(Profile_Var, Model, var_value, LL)
sigma_P_SIR_Cosine_poly_fit_model <- lm(sigma_P_poly_data_SIR_Cosine$LL ~
                                      poly(sigma_P_poly_data_SIR_Cosine$var_value,4, raw = TRUE))

sigma_P_SIR_Cosine_poly_fit_model$Poly_Fit = sigma_P_SIR_Cosine_poly_fit_model$fitted.values
small_breaks_sigma_P_SIR_Cosine = seq(from= min(sigma_P_poly_data_SIR_Cosine$var_value),
                                  to = max(sigma_P_poly_data_SIR_Cosine$var_value), length = 10^3)
sigma_P_SIR_Cosine_poly_intercept =summary(sigma_P_SIR_Cosine_poly_fit_model)$coefficients[1,1]
sigma_P_SIR_Cosine_poly_order_1 = summary(sigma_P_SIR_Cosine_poly_fit_model)$coefficients[2,1]
sigma_P_SIR_Cosine_poly_order_2 = summary(sigma_P_SIR_Cosine_poly_fit_model)$coefficients[3,1]
sigma_P_SIR_Cosine_poly_order_3 = summary(sigma_P_SIR_Cosine_poly_fit_model)$coefficients[4,1]
sigma_P_SIR_Cosine_poly_order_4 = summary(sigma_P_SIR_Cosine_poly_fit_model)$coefficients[5,1]

sigma_P_SIR_Cosine_poly_curve = sigma_P_SIR_Cosine_poly_intercept +
  sigma_P_SIR_Cosine_poly_order_1*small_breaks_sigma_P_SIR_Cosine +
  sigma_P_SIR_Cosine_poly_order_2*I(small_breaks_sigma_P_SIR_Cosine^2) +
  sigma_P_SIR_Cosine_poly_order_3*I(small_breaks_sigma_P_SIR_Cosine^3) +
  sigma_P_SIR_Cosine_poly_order_4*I(small_breaks_sigma_P_SIR_Cosine^4)
sigma_P_SIR_Cosine_poly_curve_df = data.frame(small_breaks = small_breaks_sigma_P_SIR_Cosine,
                               poly_curve = sigma_P_SIR_Cosine_poly_curve,
                               plot_var_label = "sigma_P",
                               model_var_label = "SIR Cosine")

#### sigma_P Profile SIR Spline Model
sigma_P_poly_data_SIR_Spline = Sup_Fig_5_ABC_plot_data %>%
  filter(Profile_Var == "sigma_P") %>%
  filter(Model == "A_6") %>%
  dplyr::select(Profile_Var, Model, var_value, LL)
sigma_P_SIR_Spline_poly_fit_model <- lm(sigma_P_poly_data_SIR_Spline$LL ~
                                      poly(sigma_P_poly_data_SIR_Spline$var_value,4, raw = TRUE))

sigma_P_SIR_Spline_poly_fit_model$Poly_Fit = sigma_P_SIR_Spline_poly_fit_model$fitted.values
small_breaks_sigma_P_SIR_Spline = seq(from= min(sigma_P_poly_data_SIR_Spline$var_value),
                                  to = max(sigma_P_poly_data_SIR_Spline$var_value), length = 10^3)
sigma_P_SIR_Spline_poly_intercept =summary(sigma_P_SIR_Spline_poly_fit_model)$coefficients[1,1]
sigma_P_SIR_Spline_poly_order_1 = summary(sigma_P_SIR_Spline_poly_fit_model)$coefficients[2,1]
sigma_P_SIR_Spline_poly_order_2 = summary(sigma_P_SIR_Spline_poly_fit_model)$coefficients[3,1]
sigma_P_SIR_Spline_poly_order_3 = summary(sigma_P_SIR_Spline_poly_fit_model)$coefficients[4,1]
sigma_P_SIR_Spline_poly_order_4 = summary(sigma_P_SIR_Spline_poly_fit_model)$coefficients[5,1]

sigma_P_SIR_Spline_poly_curve = sigma_P_SIR_Spline_poly_intercept +
  sigma_P_SIR_Spline_poly_order_1*small_breaks_sigma_P_SIR_Spline +
  sigma_P_SIR_Spline_poly_order_2*I(small_breaks_sigma_P_SIR_Spline^2) +
  sigma_P_SIR_Spline_poly_order_3*I(small_breaks_sigma_P_SIR_Spline^3) +
  sigma_P_SIR_Spline_poly_order_4*I(small_breaks_sigma_P_SIR_Spline^4)
sigma_P_SIR_Spline_poly_curve_df = data.frame(small_breaks = small_breaks_sigma_P_SIR_Spline,
                               poly_curve = sigma_P_SIR_Spline_poly_curve,
                               plot_var_label = "sigma_P",
                               model_var_label = "SIR Spline")

#### sigma_P Profile SEIR Spline Model
sigma_P_poly_data_SEIR_Spline = Sup_Fig_5_ABC_plot_data %>%
  filter(Profile_Var == "sigma_P") %>%
  filter(Model == "A_5") %>%
  dplyr::select(Profile_Var, Model, var_value, LL)
sigma_P_SEIR_Spline_poly_fit_model <- lm(sigma_P_poly_data_SEIR_Spline$LL ~
                                      poly(sigma_P_poly_data_SEIR_Spline$var_value,4, raw = TRUE))

sigma_P_SEIR_Spline_poly_fit_model$Poly_Fit = sigma_P_SEIR_Spline_poly_fit_model$fitted.values
small_breaks_sigma_P_SEIR_Spline = seq(from= min(sigma_P_poly_data_SEIR_Spline$var_value),
                                  to = max(sigma_P_poly_data_SEIR_Spline$var_value), length = 10^3)
sigma_P_SEIR_Spline_poly_intercept =summary(sigma_P_SEIR_Spline_poly_fit_model)$coefficients[1,1]
sigma_P_SEIR_Spline_poly_order_1 = summary(sigma_P_SEIR_Spline_poly_fit_model)$coefficients[2,1]
sigma_P_SEIR_Spline_poly_order_2 = summary(sigma_P_SEIR_Spline_poly_fit_model)$coefficients[3,1]
sigma_P_SEIR_Spline_poly_order_3 = summary(sigma_P_SEIR_Spline_poly_fit_model)$coefficients[4,1]
sigma_P_SEIR_Spline_poly_order_4 = summary(sigma_P_SEIR_Spline_poly_fit_model)$coefficients[5,1]

sigma_P_SEIR_Spline_poly_curve = sigma_P_SEIR_Spline_poly_intercept +
  sigma_P_SEIR_Spline_poly_order_1*small_breaks_sigma_P_SEIR_Spline +
  sigma_P_SEIR_Spline_poly_order_2*I(small_breaks_sigma_P_SEIR_Spline^2) +
  sigma_P_SEIR_Spline_poly_order_3*I(small_breaks_sigma_P_SEIR_Spline^3) +
  sigma_P_SEIR_Spline_poly_order_4*I(small_breaks_sigma_P_SEIR_Spline^4)
sigma_P_SEIR_Spline_poly_curve_df = data.frame(small_breaks = small_breaks_sigma_P_SEIR_Spline,
                                          poly_curve = sigma_P_SEIR_Spline_poly_curve,
                                          plot_var_label = "sigma_P",
                                          model_var_label = "SEIR Spline")

### Combine poly fit data

combined_poly_data = rbind(sigma_P_SIR_Cosine_poly_curve_df, sigma_P_SIR_Spline_poly_curve_df)
combined_poly_data = rbind(combined_poly_data, sigma_P_SEIR_Spline_poly_curve_df)

rahul_big_panel_theme = theme(
  axis.title.x = element_text(size = 14,
                              face = "bold",
                              color = "black"),
  axis.text.x = element_text(size = 12,
                             face = "bold",
                             color = "black"),
  axis.title.y = element_text(size = 14,
                              face = "bold",
                              color = "black"),
  legend.title = element_text(size = 14,
                              face = "bold",
                              color = "black"),
  legend.text = element_text(size = 12,
                             face = "bold",
                             color = "black"),
  axis.text.y = element_text(size = 12,
                             face = "bold",
                             color = "black"),
  plot.margin = unit(c(.5,.5,.5,.5), "cm"),
  legend.background = element_rect(fill = "transparent"),
  legend.box.margin = unit(c(.5,.5,.5,.5), "cm")
)

### Plot Figure S5
Sup_Fig_5_combined_data$model_var_label = factor(Sup_Fig_5_combined_data$model_var_label, levels = c("SIR Cosine", "SIR Spline",
                                                       "SEIR Spline"))
Fig_S5_comb_plot = ggplot()  + geom_point(data = Sup_Fig_5_combined_data, aes(x = var_value, y = LL, color = Metric, shape = Metric)) +
  scale_linetype_manual(values = c("blank", "solid")) +
  rahul_man_figure_theme + rahul_big_panel_theme  +theme_white_background  +
  geom_hline(data = Sup_Fig_5_combined_data, aes(yintercept = ML -2), size = 1.0, linetype = "dashed", color = 'grey70') +
  geom_vline(data = Sup_Fig_5_combined_data, aes(xintercept = MLE_value_for_prof_var),
             size = 1.0, linetype = "twodash",show.legend= F, color = 'grey70') +
  facet_wrap(~model_var_label, scales = "free", 
             ncol = 1) +
  geom_hline(data = Sup_Fig_5_combined_data, aes(yintercept = low_bound), color = 'white', linetype = 'blank') +
  geom_line(data = combined_poly_data, aes(x = small_breaks, y = poly_curve), color = 'red', show.legend = F) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    strip.placement = "outside"
  ) + 
  theme(legend.position = "None") +
  scale_color_manual(values = c("black", "red", "black", "white"),
                     limits = c("LL", "Skips", "Show_Line", "No_Line")) +
  scale_shape_manual(values = c(16, 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4))  +
  ylab(expression(paste(" Log Likelihood "))) +
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(face = "plain"),
        strip.text = element_text(face = "plain")) +
  theme(panel.spacing = unit(1.75, "lines")) +
  xlab(expression(sigma_P))

#theme(axis.title.x=element_blank()) + 
Fig_S5_comb_plot

pdf(
  paste0(
    "../Figures/Supplemental_Figures/Supplemental_Figure_5/Supplemental_Figure_5.pdf"),
  height = 10, width = 5)
Fig_S5_comb_plot
dev.off()

