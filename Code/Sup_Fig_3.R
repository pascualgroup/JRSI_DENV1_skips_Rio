rm(list = ls())
source("load_libraries_essential.R")
library(zoo)
library(pomp)
source("rahul_theme.R")

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
                    "A_5", "A_3" )
model_label_list = factor(c("SIR Cosine No Immigration", "SIR Spline No Immmigration",
                            "SEIR Spline No Immigration", "SEIR Spline Immigration" ))
model_label_list = factor(model_label_list, levels = c("SIR Cosine No Immigration", "SIR Spline No Immmigration",
                                                       "SEIR Spline No Immigration", "SEIR Spline Immigration" ))

Csnippet_file_path_list = c("Csnippet_SIR_cosine_model.R",
                            "Csnippet_SIR_spline_model.R",
                            "Csnippet_SEIR_spline_model.R",
                            "Csnippet_SEIR_spline_model.R")
Num_est_parameters_list = c(7,7,8,9)
data_file_path_list = c("../Generated_Data/Rio_DENV1_Data_2_25_years_clean.csv",
                        "../Generated_Data/Rio_DENV1_Data_2_25_years_clean.csv",
                        "../Generated_Data/Rio_DENV1_Data_2_25_years_clean.csv",
                        "../Generated_Data/Rio_DENV1_Data_3_75_years_clean.csv")

num_years_list = c(2.50, 2.50, 2.50, 4)

model_ref_df = data.frame(model_name = model_name_list, model_label = model_label_list,
                          Csnippet_file_path = Csnippet_file_path_list,
                          Num_est_parameters = Num_est_parameters_list,
                          data_file_path = data_file_path_list,
                          num_years = num_years_list,stringsAsFactors = FALSE)

Sup_Figure_3_A_B_df_colnames = c("time", "ML_high_Q", "ML_low_Q",
                                 "all_combo_high_Q_max","all_combo_low_Q_min","all_combo_median_min",
                                 "all_combo_median_max","all_combo_median_max", "variable",
                                 "value","ML_Q_Rib_Col","All_combo_Med_Rib_Col","All_combo_Q_Rib_Col",
                                 "Model", "Model_Name")

Sup_Figure_3_C_df_colnames = c("time", "ML_high_Q", "ML_low_Q", "all_combo_high_Q_max",
                               "all_combo_low_Q_min", "all_combo_median_min", "all_combo_median_max", "variable",
                               "value", "ML_Q_Rib_Col","All_combo_Med_Rib_Col","All_combo_Q_Rib_Col",
                               "Model","Model_Name")

Sup_Figure_3_D_df_colnames = c("time", "R_0", "R_0_min", "R_0_max", "Year", "Days_in_Year", "Month", "Month_Name", "Model", "Model_Name")
Sup_Figure_3_D_label_df_colnames = c("plot_label_times", "plot_label_month_names", "Model", "Model_Name")

Sup_Figure_3_E_df_colnames = c("time", "ML_high_Q", "ML_low_Q", "all_combo_high_Q_max",
                               "all_combo_low_Q_min", "all_combo_median_min", "all_combo_median_max", "variable",
                               "value", "ML_Q_Rib_Col","All_combo_Med_Rib_Col","All_combo_Q_Rib_Col",
                               "Model","Model_Name")

Sup_Figure_3_A_B_df = data.frame(matrix(nrow = 0, ncol = length(Sup_Figure_3_A_B_df_colnames)))
Sup_Figure_3_C_df = data.frame(matrix(nrow = 0, ncol = length(Sup_Figure_3_C_df_colnames)))
Sup_Figure_3_D_df = data.frame(matrix(nrow = 0, ncol = length(Sup_Figure_3_D_df_colnames)))
Sup_Figure_3_E_df = data.frame(matrix(nrow = 0, ncol = length(Sup_Figure_3_E_df_colnames)))

Sup_Figure_3_D_label_df = data.frame(matrix(nrow = 0, ncol = length(Sup_Figure_3_D_label_df_colnames)))

colnames(Sup_Figure_3_A_B_df) = Sup_Figure_3_A_B_df_colnames
colnames(Sup_Figure_3_C_df) = Sup_Figure_3_C_df_colnames
colnames(Sup_Figure_3_D_df) = Sup_Figure_3_D_df_colnames
colnames(Sup_Figure_3_D_label_df) = Sup_Figure_3_D_label_df_colnames
colnames(Sup_Figure_3_E_df) = Sup_Figure_3_E_df_colnames


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
  
  all_combo_data = read.csv(paste0("../Generated_Data/Profiles/", model_name, "_Model/",model_name, "_Model_BP_top_2_LL_all_params_sim_cases_data.csv"))
  
  all_R0_data = read.csv(paste0("../Generated_Data/Profiles/", model_name, "_Model/",model_name, "_Model_BP_top_2_LL_all_params_sim_R0_data.csv"))
  
  all_combo_S_data = read.csv(paste0("../Generated_Data/Profiles/", model_name, "_Model/",model_name, "_Model_BP_top_2_LL_all_params_sim_S_over_N_data.csv"))
  
  all_R_eff_data = read.csv(paste0("../Generated_Data/Profiles/", model_name, "_Model/",model_name, "_Model_BP_top_2_LL_all_params_sim_Reff_data.csv"))
  
  bio_good_2_LL = read.csv(paste0("../Generated_Data/Profiles/", model_name, "_Model/",model_name, "_Model_BP_top_2_LL_all_params_bio_good_2_LL.csv"))
  
  # Get data for Sup Figure 2A and 2B
  all_combo_data_high_Q_max = aggregate(sim_data_high_Q ~ time, all_combo_data,
                                        FUN = max)
  all_combo_data_high_Q_max = dplyr::select(all_combo_data_high_Q_max,
                                            time = time,
                                            all_combo_high_Q_max = sim_data_high_Q)
  all_combo_data_low_Q_min = aggregate(sim_data_low_Q ~ time, all_combo_data,
                                       FUN = min)
  all_combo_data_low_Q_min = dplyr::select(all_combo_data_low_Q_min,
                                           time = time,
                                           all_combo_low_Q_min = sim_data_low_Q)
  all_combo_data_median_max = aggregate(sim_data_median ~ time, all_combo_data,
                                        FUN = max)
  
  all_combo_data_median_max = dplyr::select(all_combo_data_median_max,
                                            time = time,
                                            all_combo_median_max = sim_data_median)
  
  all_combo_data_median_min = aggregate(sim_data_median ~ time, all_combo_data,
                                        FUN = min)
  all_combo_data_median_min = dplyr::select(all_combo_data_median_min,
                                            time = time,
                                            all_combo_median_min = sim_data_median)
  
  ML_combo_num = which(bio_good_2_LL$LL == max(bio_good_2_LL$LL))
  
  ML_output = filter(all_combo_data, combo_num == ML_combo_num)
  ML_output = dplyr::select(ML_output, time = time,
                            ML_median = sim_data_median,
                            ML_high_Q = sim_data_high_Q,
                            ML_low_Q = sim_data_low_Q)
  
  comp_data = join(ML_output, all_combo_data_high_Q_max)
  
  comp_data = join(comp_data, all_combo_data_low_Q_min)
  
  comp_data = join(comp_data, all_combo_data_median_min)
  
  comp_data = join(comp_data, all_combo_data_median_max)
  
  true_data = dplyr::select(Rio_clean_data, time = times,
                            Observed_Data = Y)
  comp_data = join(comp_data, true_data)
  
  comp_data_melt = melt(comp_data, id.vars = c("time",
                                               "ML_high_Q", "ML_low_Q",
                                               "all_combo_high_Q_max",
                                               "all_combo_low_Q_min",
                                               "all_combo_median_min",
                                               "all_combo_median_max"))
  
  
  
  
  
  comp_data_melt$ML_Q_Rib_Col = "95% Simulation Quantiles \n (MLE)"
  comp_data_melt$All_combo_Med_Rib_Col = "Simulation Median \n  (all 2 LL combinations)"
  comp_data_melt$All_combo_Q_Rib_Col = "95% Simulation Quantiles \n (all 2 LL combinations)"
  
  comp_data_melt$Model = model_name
  comp_data_melt$Model_Name = model_label
  
  Sup_Figure_3_A_B_df = rbind(Sup_Figure_3_A_B_df, comp_data_melt)
  ## Data for Supplemental Figure 2C
  all_combo_data_high_Q_max = aggregate(sim_data_S_over_N_high_Q ~ time, all_combo_S_data,
                                        FUN = max)
  all_combo_data_high_Q_max = dplyr::select(all_combo_data_high_Q_max,
                                            time = time,
                                            all_combo_high_Q_max = sim_data_S_over_N_high_Q)
  all_combo_data_low_Q_min = aggregate(sim_data_S_over_N_low_Q ~ time, all_combo_S_data,
                                       FUN = min)
  all_combo_data_low_Q_min = dplyr::select(all_combo_data_low_Q_min,
                                           time = time,
                                           all_combo_low_Q_min = sim_data_S_over_N_low_Q)
  all_combo_data_median_max = aggregate(sim_data_S_over_N_median ~ time, all_combo_S_data,
                                        FUN = max)
  
  all_combo_data_median_max = dplyr::select(all_combo_data_median_max,
                                            time = time,
                                            all_combo_median_max = sim_data_S_over_N_median)
  
  all_combo_data_median_min = aggregate(sim_data_S_over_N_median ~ time, all_combo_S_data,
                                        FUN = min)
  all_combo_data_median_min = dplyr::select(all_combo_data_median_min,
                                            time = time,
                                            all_combo_median_min = sim_data_S_over_N_median)
  
  ML_combo_num = which(bio_good_2_LL$LL == max(bio_good_2_LL$LL))
  
  ML_output = filter(all_combo_S_data, combo_num == ML_combo_num)
  ML_output = dplyr::select(ML_output, time = time,
                            ML_median = sim_data_S_over_N_median,
                            ML_high_Q = sim_data_S_over_N_high_Q,
                            ML_low_Q = sim_data_S_over_N_low_Q)
  
  comp_data = join(ML_output, all_combo_data_high_Q_max)
  comp_data = join(comp_data, all_combo_data_low_Q_min)
  comp_data = join(comp_data, all_combo_data_median_min)
  comp_data = join(comp_data, all_combo_data_median_max)
  
  comp_data_melt = melt(comp_data, id.vars = c("time",
                                               "ML_high_Q", "ML_low_Q",
                                               "all_combo_high_Q_max",
                                               "all_combo_low_Q_min",
                                               "all_combo_median_min",
                                               "all_combo_median_max"))
  ML_Q_Rib_Col_lab = "95% Simulation Quantiles \n (MLE)"
  All_combo_Med_Rib_Col_lab = "Simulation Median \n  (all 2 LL combinations)"
  All_combo_Q_Rib_Col_lab = "95% Simulation Quantiles \n (all 2 LL combinations)"
  comp_data_melt$ML_Q_Rib_Col = ML_Q_Rib_Col_lab
  comp_data_melt$All_combo_Med_Rib_Col = All_combo_Med_Rib_Col_lab
  comp_data_melt$All_combo_Q_Rib_Col = All_combo_Q_Rib_Col_lab
  
  comp_data_melt$Model = model_name
  comp_data_melt$Model_Name = model_label
  
  Sup_Figure_3_C_df = rbind(Sup_Figure_3_C_df, comp_data_melt)
  
  
  ## Data for Supplemental Figure 2D
  R_0_max = aggregate(R_0~time, all_R0_data, FUN = max)
  R_0_max = dplyr::select(R_0_max, time = time, R_0_max = R_0)
  R_0_min = aggregate(R_0~time, all_R0_data, FUN = min)
  R_0_min = dplyr::select(R_0_min, time = time, R_0_min = R_0)
  
  ML_R0_df = filter(all_R0_data, combo_num == ML_combo_num)
  ML_R0_df = dplyr::select(ML_R0_df, time = time, R_0_MLE = R_0)
  R_0_ribbon_df = join(R_0_min, R_0_max)
  R_0_ribbon_df = join(R_0_ribbon_df, ML_R0_df)
  
  R_0_ribbon_df_melt = melt(R_0_ribbon_df, id.vars = c("time", "R_0_min", "R_0_max" ))
  ribbon_label_2_D = "R_0 range \n  (All  2 LL Combinations)"
  
  R_0_ribbon_df_melt$Ribbon_label = ribbon_label_2_D
  
  
  R_0_plot_data = R_0_ribbon_df_melt
  
  R_0_plot_data$Year = R_0_plot_data$time/365
  R_0_plot_data$Days_in_Year = (R_0_plot_data$time%%365)
  R_0_plot_data$Month = round((R_0_plot_data$Days_in_Year/365)*12) + 1
  single_year_R_0_data= filter(R_0_plot_data, Year <=  2 & Year >= 1 )
  single_year_R_0_data$Month_Name = c(month.abb, month.abb[1])
  
  single_year_R_0_data$Model = model_name
  single_year_R_0_data$Model_Name = model_label
  Sup_Figure_3_D_df = rbind(Sup_Figure_3_D_df, single_year_R_0_data)
  
  plot_label_months =seq(from = 1, to = 13, by = 2)
  plot_label_month_names = single_year_R_0_data$Month_Name[plot_label_months]
  plot_label_times = single_year_R_0_data$time[plot_label_months]
  
  plot_label_months = seq(from = 1.00, to = 13.00, length = 7)
  
  single_model_Sup_Figure_3_D_label_df = data.frame(plot_label_times, plot_label_month_names)
  single_model_Sup_Figure_3_D_label_df$Model = model_name
  single_model_Sup_Figure_3_D_label_df$Model_Name = model_label
  
  Sup_Figure_3_D_label_df = rbind(Sup_Figure_3_D_label_df, single_model_Sup_Figure_3_D_label_df)
  
  ## Data for Supplemental Figure 2E
  all_combo_data_high_Q_max = aggregate(R_eff_high_Q ~ time, all_R_eff_data,
                                        FUN = max)
  all_combo_data_high_Q_max = dplyr::select(all_combo_data_high_Q_max,
                                            time = time,
                                            all_combo_high_Q_max = R_eff_high_Q)
  all_combo_data_low_Q_min = aggregate(R_eff_low_Q ~ time, all_R_eff_data,
                                       FUN = min)
  all_combo_data_low_Q_min = dplyr::select(all_combo_data_low_Q_min,
                                           time = time,
                                           all_combo_low_Q_min = R_eff_low_Q)
  
  all_combo_data_median_max = aggregate(R_eff_median ~ time, all_R_eff_data,
                                        FUN = max)
  
  all_combo_data_median_max = dplyr::select(all_combo_data_median_max,
                                            time = time,
                                            all_combo_median_max = R_eff_median)
  
  all_combo_data_median_min = aggregate(R_eff_median ~ time, all_R_eff_data,
                                        FUN = min)
  all_combo_data_median_min = dplyr::select(all_combo_data_median_min,
                                            time = time,
                                            all_combo_median_min = R_eff_median)
  ML_combo_num = which(bio_good_2_LL$LL == max(bio_good_2_LL$LL))
  
  ML_output = filter(all_R_eff_data, combo_num == ML_combo_num)
  ML_output = dplyr::select(ML_output, time = time,
                            ML_median = R_eff_median,
                            ML_high_Q = R_eff_high_Q,
                            ML_low_Q = R_eff_low_Q)
  
  comp_data = join(ML_output, all_combo_data_high_Q_max)
  comp_data = join(comp_data, all_combo_data_low_Q_min)
  comp_data = join(comp_data, all_combo_data_median_min)
  comp_data = join(comp_data, all_combo_data_median_max)
  comp_data_melt = melt(comp_data, id.vars = c("time",
                                               "ML_high_Q", "ML_low_Q",
                                               "all_combo_high_Q_max",
                                               "all_combo_low_Q_min",
                                               "all_combo_median_min",
                                               "all_combo_median_max"))
  ML_Q_Rib_Col_lab = "95% Simulation Quantiles \n (MLE)"
  All_combo_Med_Rib_Col_lab = "Simulation Median \n  (all 2 LL combinations)"
  All_combo_Q_Rib_Col_lab = "95% Simulation Quantiles \n (all 2 LL combinations)"
  comp_data_melt$ML_Q_Rib_Col = ML_Q_Rib_Col_lab
  comp_data_melt$All_combo_Med_Rib_Col = All_combo_Med_Rib_Col_lab
  comp_data_melt$All_combo_Q_Rib_Col = All_combo_Q_Rib_Col_lab
  
  comp_data_melt$Model = model_name
  comp_data_melt$Model_Name = model_label
  
  Sup_Figure_3_E_df = rbind(Sup_Figure_3_E_df, comp_data_melt)
  
}


# Make plots --------------------------------------------------------------

fill_vec_3_A_B = c("Simulation Median \n  (all 2 LL combinations)" = "pink", "95% Simulation Quantiles \n (MLE)" = "skyblue", "95% Simulation Quantiles \n (all 2 LL combinations)" = "grey70")

fill_vec_C = c("pink", "skyblue", "grey70")
names(fill_vec_C) = c(All_combo_Med_Rib_Col_lab, ML_Q_Rib_Col_lab, All_combo_Q_Rib_Col_lab)

fill_vec_3_D = c("grey70")
names(fill_vec_3_D) = ribbon_label_2_D

fill_vec_3E = c("pink", "skyblue", "grey70")
names(fill_vec_3E) = c(All_combo_Med_Rib_Col_lab, ML_Q_Rib_Col_lab, All_combo_Q_Rib_Col_lab)

Sup_Fig_3_A = ggplot(data = Sup_Figure_3_A_B_df) +
  geom_ribbon(aes(x = time/365, ymin = all_combo_low_Q_min,
                  ymax = all_combo_high_Q_max, fill = All_combo_Q_Rib_Col), inherit.aes = FALSE) +
  geom_ribbon(aes(x = time/365, ymin = ML_low_Q,
                  ymax = ML_high_Q, fill = ML_Q_Rib_Col),  inherit.aes = FALSE) +
  geom_ribbon(aes(x = time/365, ymin = all_combo_median_min,
                  ymax = all_combo_median_max, fill = All_combo_Med_Rib_Col), inherit.aes = FALSE) +
  geom_line(aes(x = time/365, y = value, color = variable)) +
  geom_point(aes(x = time/365, y = value, color = variable)) +
  rahul_theme +
  theme(legend.text = element_text(size = 12,
                                   face = "bold",
                                   color = "black")) +
  theme_white_background +
  
  scale_fill_manual(name = "Ribbon  Legend", values = fill_vec_3_A_B) +
  scale_color_manual(name = "Color  Legend", values = c("red","blue"),
                     labels =
                       c("Simulation Median \n (MLE)",
                         "Observed"))  +
  xlab("Years since Jan 1 1986")+
  ylab("Observed Monthly Cases")+
  facet_wrap(~Model_Name, ncol = 1)
Sup_Fig_3_A

pdf("../Figures/Supplemental_Figures/Supplemental_Figure_3/Supplemental_Figure_3A.pdf")
print(Sup_Fig_3_A)
dev.off()

Sup_Fig_3_B = ggplot(data = Sup_Figure_3_A_B_df) +
  geom_ribbon(aes(x = time/365, ymin = log(all_combo_low_Q_min),
                  ymax = log(all_combo_high_Q_max), fill = All_combo_Q_Rib_Col), inherit.aes = FALSE) +
  geom_ribbon(aes(x = time/365, ymin = log(ML_low_Q),
                  ymax = log(ML_high_Q), fill = ML_Q_Rib_Col),  inherit.aes = FALSE) +
  geom_ribbon(aes(x = time/365, ymin = log(all_combo_median_min),
                  ymax = log(all_combo_median_max), fill = All_combo_Med_Rib_Col), inherit.aes = FALSE) +
  geom_line(aes(x = time/365, y = log(value), color = variable)) +
  geom_point(aes(x = time/365, y = log(value), color = variable)) +
  rahul_theme +
  theme(legend.text = element_text(size = 10,
                                   face = "bold",
                                   color = "black")) +
  theme(legend.title = element_text(size = 10,
                                   face = "bold",
                                   color = "black")) +
  theme_white_background +
  scale_fill_manual(name = "Ribbon  Legend", values = fill_vec_3_A_B) +
  scale_color_manual(name = "Color  Legend", values = c("red","blue"),
                     labels =
                       c("Simulation Median \n (MLE)",
                         "Observed"))  +
  xlab("Years since Jan 1 1986")+
  ylab("log(Observed Monthly Cases) ")+
  facet_wrap(~Model_Name, ncol = 1)
Sup_Fig_3_B
pdf("../Figures/Supplemental_Figures/Supplemental_Figure_3/Supplemental_Figure_3B.pdf")
print(Sup_Fig_3_B)
dev.off()

Sup_Fig_3_C = ggplot(data = Sup_Figure_3_C_df) +
  geom_ribbon(aes(x = time/365, ymin = all_combo_low_Q_min,
                  ymax = all_combo_high_Q_max, fill = All_combo_Q_Rib_Col), inherit.aes = FALSE) +
  geom_ribbon(aes(x = time/365, ymin = all_combo_median_min,
                  ymax = all_combo_median_max, fill = All_combo_Med_Rib_Col), inherit.aes = FALSE) +
  geom_ribbon(aes(x = time/365, ymin = ML_low_Q,
                  ymax = ML_high_Q, fill = ML_Q_Rib_Col),  inherit.aes = FALSE) +
  geom_line(aes(x = time/365, y = value, color = variable)) +
  geom_point(aes(x = time/365, y = value, color = variable)) +
  rahul_theme +
  theme(legend.text = element_text(size = 12,
                                   face = "bold",
                                   color = "black")) +
  theme_white_background +
  
  scale_fill_manual(name = "Ribbon  Legend", values = fill_vec_C) +
  scale_color_manual(name = "Color  Legend", values = c("red"),
                     labels =
                       c("Simulation Median \n (MLE)"))  +
  xlab("Years since Jan 1 1986")+
  ylab(expression(paste(frac(S,N))))+
  facet_wrap(~Model_Name, ncol = 1)
Sup_Fig_3_C
pdf("../Figures/Supplemental_Figures/Supplemental_Figure_3/Supplemental_Figure_3C.pdf")
print(Sup_Fig_3_C)
dev.off()


Sup_Fig_3_D = ggplot(data = Sup_Figure_3_D_df) +
  geom_ribbon(aes(x = time, ymin = R_0_min, ymax =  R_0_max, fill = ribbon_label_2_D)) +
  geom_line(aes(x = time, y = value, color = variable)) +
  geom_point(aes(x = time, y = value, color = variable)) +
  rahul_theme +
  theme(legend.text = element_text(size = 12,
                                   face = "bold",
                                   color = "black")) +
  theme_white_background +
  scale_fill_manual(name = "Ribbon  Legend", values = fill_vec_3_D) +
  scale_color_manual(name = "Color  Legend", values = c("red"),
                     labels = c("R_0 (MLE)"))  +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Month") + ylab(expression(R[0])) +
  scale_x_continuous(breaks = Sup_Figure_3_D_label_df$plot_label_times,
                     labels = Sup_Figure_3_D_label_df$plot_label_month_names)+
  facet_wrap(~Model_Name, ncol = 1)
Sup_Fig_3_D
pdf("../Figures/Supplemental_Figures/Supplemental_Figure_3/Supplemental_Figure_3D.pdf")
print(Sup_Fig_3_D)
dev.off()

## Supplemental Figure 2E
Sup_Fig_3_E = ggplot(data = Sup_Figure_3_E_df) +
  geom_ribbon(aes(x = time/365, ymin = all_combo_low_Q_min,
                  ymax = all_combo_high_Q_max, fill = All_combo_Q_Rib_Col), inherit.aes = FALSE) +
  geom_ribbon(aes(x = time/365, ymin = all_combo_median_min,
                  ymax = all_combo_median_max, fill = All_combo_Med_Rib_Col), inherit.aes = FALSE) +
  geom_ribbon(aes(x = time/365, ymin = ML_low_Q,
                  ymax = ML_high_Q, fill = ML_Q_Rib_Col),  inherit.aes = FALSE) +
  geom_line(aes(x = time/365, y = value, color = variable)) +
  geom_point(aes(x = time/365, y = value, color = variable)) +
  rahul_theme +
  theme(legend.text = element_text(size = 12,
                                   face = "bold",
                                   color = "black")) +
  theme_white_background +
  
  scale_fill_manual(name = "Ribbon  Legend", values = fill_vec_3E) +
  scale_color_manual(name = "Color  Legend", values = c("red"),
                     labels =
                       c("Simulation Median \n (MLE)"))  +
  xlab("Years since Jan 1 1986")+
  ylab(expression(paste(R[eff])))+
  facet_wrap(~Model_Name, ncol = 1)
Sup_Fig_3_E
pdf("../Figures/Supplemental_Figures/Supplemental_Figure_3/Supplemental_Figure_3E.pdf")
print(Sup_Fig_3_E)
dev.off()

# Combined Plot -----------------------------------------------------------

library(gridExtra)
library(grid)
library(lattice)

rahul_panel_theme = theme(
  axis.title.x = element_text(size = 10,
                              face = "bold",
                              color = "black"),
  axis.text.x = element_text(size = 9,
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
                             color = "black"),
  strip.text = element_text(size = 8.5,
                            face = "plain",
                            color = "black")
)

Sup_Fig_3_A_comb = Sup_Fig_3_A + rahul_panel_theme + theme(legend.position = "None")
Sup_Fig_3_B_comb = Sup_Fig_3_B + rahul_panel_theme + theme(legend.position = "None")
Sup_Fig_3_C_comb = Sup_Fig_3_C + rahul_panel_theme + theme(legend.position = "None")
Sup_Fig_3_D_comb = Sup_Fig_3_D + rahul_panel_theme + theme(legend.position = "None")
Sup_Fig_3_E_comb = Sup_Fig_3_E + rahul_panel_theme + theme(legend.position = "None")
Sup_Fig_3_A_small_legend = Sup_Fig_3_A + rahul_panel_theme
legend <- cowplot::get_legend(Sup_Fig_3_A_small_legend)

Sup_Fig_3_A_leg = grid.draw(legend)




pdf("../Figures/Supplemental_Figures/Supplemental_Figure_3/Supplemental_Figure_3_Combined.pdf")
print(grid.arrange(Sup_Fig_3_A_comb, Sup_Fig_3_B_comb, Sup_Fig_3_C_comb, Sup_Fig_3_D_comb, Sup_Fig_3_E_comb,
                   legend, ncol = 3))
dev.off()


