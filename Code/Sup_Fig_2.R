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
                    "A_5")
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

Sup_Figure_2_A_B_df_colnames = c("time", "sim_data_low_Q", "sim_data_high_Q", "variable", "value", "Model", "Model_Name")


Sup_Figure_2_A_B_df = data.frame(matrix(nrow = 0, ncol = length(Sup_Figure_2_A_B_df_colnames)))



colnames(Sup_Figure_2_A_B_df) = Sup_Figure_2_A_B_df_colnames


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
  
  Rio_data_first_two_and_half_years_only = filter(Rio_data_clean, times <= 365 *
                                                    num_years)
  
  
  
  
  ML_params$r = 0
  sim_data = simulate(nsim = 100,
                      seed = 12345,
                      times = Rio_data_first_two_and_half_years_only$times,
                      t0 = t0,
                      rprocess = euler(rproc,delta.t = 1),
                      params = ML_params,
                      paramnames = paramnames,
                      statenames = statenames,
                      obsnames = obsnames,
                      accumvars = acumvarnames,
                      covar = covar,
                      rinit = init,
                      rmeas = rmeas,
                      partrans = par_trans,
                      format = "data.frame")
  #head(sim_data)
  sim_data_median_Y = aggregate(Y ~ time, sim_data, median)
  sim_data_quant = aggregate(Y ~ time, sim_data, quantile, probs = c(0.025, 0.975))
  sim_data_quant$Y = as.data.frame(sim_data_quant$Y)
  colnames(sim_data_quant$Y) = c("Q2.5", "Q97.5")
  
  comp_data = data.frame(time = sim_data_median_Y$time,
                         sim_data_median = sim_data_median_Y$Y,
                         sim_data_low_Q = sim_data_quant$Y$Q2.5,
                         sim_data_high_Q = sim_data_quant$Y$Q97.5,
                         true_data = Rio_data_clean$Y)
  
  comp_data_melt = melt(comp_data, id.vars = c("time", "sim_data_low_Q",
                                               "sim_data_high_Q"))
  
  comp_data_melt$Model = model_name
  comp_data_melt$Model_Name = model_label
  
  single_model_case_data = comp_data_melt
  
  Sup_Figure_2_A_B_df = rbind(Sup_Figure_2_A_B_df, single_model_case_data)
  
 
  
  
  
}


# Make plots --------------------------------------------------------------


Sup_Fig_2_A = ggplot(data = Sup_Figure_2_A_B_df) +
  geom_ribbon(aes(x = time/365, ymin = log(sim_data_low_Q),
                  ymax = log(sim_data_high_Q)), fill = "red", alpha = 0.50) +
  geom_line(aes(x = time/365, y = log(value), color = variable)) +
  geom_point(aes(x = time/365, y = log(value), color = variable)) +
  rahul_theme +
  theme_white_background +
  median_legend_lab +
  xlab("Years since Jan 1 1986")+
  ylab("log(Observed Monthly Cases)")+
  facet_wrap(~Model_Name, ncol = 1, strip.position = "left") +
  theme(legend.key=element_blank()) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)) +
  theme(legend.text = element_text(size = 15.5,
                                   face = "plain"))+
  scale_color_manual(name = "",
                     values = c("red",
                                "blue"),
                     labels = c("Simulated",
                                "Observed")) 

Sup_Fig_2_A
pdf("../Figures/Supplemental_Figures/Supplemental_Figure_2/Supplemental_Figure_2A.pdf")
print(Sup_Fig_2_A)
dev.off()

Sup_Fig_2_B = ggplot(data = Sup_Figure_2_A_B_df) +
  geom_ribbon(aes(x = time/365, ymin = sim_data_low_Q,
                  ymax = sim_data_high_Q), fill = "red", alpha = 0.50) +
  geom_line(aes(x = time/365, y = value, color = variable)) +
  geom_point(aes(x = time/365, y = value, color = variable)) +
  rahul_theme +
  theme_white_background +
  median_legend_lab +
  xlab("Years since Jan 1 1986")+
  ylab("Observed Monthly Cases") +
  facet_wrap(~Model_Name, ncol = 1, strip.position = "left")+
  theme(legend.key=element_blank()) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)) +
  theme(legend.text = element_text(size = 15.5,
                                   face = "plain")) +
  scale_color_manual(name = "",
                     values = c("red",
                                "blue"),
                     labels = c("Simulated",
                                "Observed")) 
Sup_Fig_2_B
pdf("../Figures/Supplemental_Figures/Supplemental_Figure_2/Supplemental_Figure_2B.pdf")
print(Sup_Fig_2_B)
dev.off()
print(grid.arrange(Sup_Fig_2_A, Sup_Fig_2_B))






# Combined Plot -----------------------------------------------------------

library(gridExtra)
library(grid)
library(lattice)

rahul_panel_theme = theme(
  axis.title.x = element_text(size = 11,
                              face = "plain",
                              color = "black"),
  axis.text.x = element_text(size = 9,
                             face = "bold",
                             color = "black"),
  axis.title.y = element_text(size = 11,
                              face = "plain",
                              color = "black"),
  legend.title = element_blank(),
  legend.text = element_text(size = 6.5,
                             face = "plain",
                             color = "black"),
  axis.text.y = element_text(size = 10,
                             face = "bold",
                             color = "black"),
  strip.text = element_text(size = 8.5,
                            face = "plain",
                            color = "black")
)

Sup_Fig_2_A_comb = Sup_Fig_2_A + rahul_panel_theme + theme(legend.position = "None") +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    strip.placement = "outside"
  )+
  theme(strip.text = element_blank()) 
Sup_Fig_2_A_comb

Sup_Fig_2_B_comb = Sup_Fig_2_B + rahul_panel_theme + theme(legend.position = c(.75, .97)) +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    strip.placement = "outside"
  ) +
  theme(strip.text = element_blank())



Sup_Figure_2_C_df_colnames = c("time", "R_0", "R_0_min", "R_0_max", "Year", "Days_in_Year", "Month",  "Model", "Model_Name")
Sup_Figure_2_C_label_df_colnames = c("plot_label_times", "Model", "Model_Name")


Sup_Figure_2_C_df = data.frame(matrix(nrow = 0, ncol = length(Sup_Figure_2_C_df_colnames)))
Sup_Figure_2_C_label_df = data.frame(matrix(nrow = 0, ncol = length(Sup_Figure_2_C_label_df_colnames)))


colnames(Sup_Figure_2_C_df) = Sup_Figure_2_C_df_colnames
colnames(Sup_Figure_2_C_label_df) = Sup_Figure_2_C_label_df_colnames 

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
  

  
  ## Data for Supplemental Figure 2C
  R_0_max = aggregate(R_0~time, all_R0_data, FUN = max)
  R_0_max = dplyr::select(R_0_max, time = time, R_0_max = R_0)
  R_0_min = aggregate(R_0~time, all_R0_data, FUN = min)
  R_0_min = dplyr::select(R_0_min, time = time, R_0_min = R_0)
  
  ML_combo_num = which(bio_good_2_LL$LL == max(bio_good_2_LL$LL))
  
  ML_R0_df = filter(all_R0_data, combo_num == ML_combo_num)
  ML_R0_df = dplyr::select(ML_R0_df, time = time, R_0_MLE = R_0)
  R_0_ribbon_df = join(R_0_min, R_0_max)
  R_0_ribbon_df = join(R_0_ribbon_df, ML_R0_df)
  
  R_0_ribbon_df_melt = melt(R_0_ribbon_df, id.vars = c("time", "R_0_min", "R_0_max" ))
  ribbon_label_2_C = "R_0 range \n  (All  2 LL Combinations)"
  
  R_0_ribbon_df_melt$Ribbon_label = ribbon_label_2_C
  
  
  R_0_plot_data = R_0_ribbon_df_melt
  
  R_0_plot_data$Year = R_0_plot_data$time/365
  R_0_plot_data$Days_in_Year = (R_0_plot_data$time%%365)
  R_0_plot_data$Month = round((R_0_plot_data$Days_in_Year/365)*12) + 1
  all_years_R_0_data= R_0_plot_data

  all_years_R_0_data$Model = model_name
  all_years_R_0_data$Model_Name = model_label
  Sup_Figure_2_C_df = rbind(Sup_Figure_2_C_df, all_years_R_0_data)
  
  # plot_label_months =seq(from = 1, to = 13, by = 2)
  # plot_label_month_names = single_year_R_0_data$Month_Name[plot_label_months]
  # plot_label_times = single_year_R_0_data$time[plot_label_months]
  # 
  # plot_label_months = seq(from = 1.00, to = 13.00, length = 7)
  # 
  # 

 
}

fill_vec_2_C = c("grey70")
names(fill_vec_2_C) = ribbon_label_2_C



Sup_Fig_2_C = ggplot(data = Sup_Figure_2_C_df) +
  geom_ribbon(aes(x = time, ymin = R_0_min, ymax =  R_0_max, fill = ribbon_label_2_C)) +
  geom_line(aes(x = time, y = value, color = variable)) +
  geom_point(aes(x = time, y = value, color = variable)) +
  rahul_theme +
  theme(legend.text = element_text(size = 12,
                                   face = "bold",
                                   color = "black")) +
  theme_white_background +
  scale_fill_manual(name = "Ribbon  Legend", values = fill_vec_2_C) +
  scale_color_manual(name = "Color  Legend", values = c("black"),
                     labels = c("R_0 (MLE)"))  +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Month") + ylab(expression(R[0])) +
  facet_wrap(~Model_Name, ncol = 1, strip.position = "left")+
  theme(legend.key=element_blank()) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 20,
                                    face = "plain"),
        axis.title.y = element_text(size = 20,
                                    face = "plain")) +
  theme(legend.text = element_text(size = 15.5,
                                   face = "plain"))
Sup_Fig_2_C
pdf("../Figures/Supplemental_Figures/Supplemental_Figure_2/Supplemental_Figure_2C.pdf")
print(Sup_Fig_2_C)
dev.off()

Sup_Fig_2_C_comb = Sup_Fig_2_C + rahul_panel_theme + theme(legend.position = "None") +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    strip.placement = "outside"
  ) +
  theme(strip.text = element_blank())


tiff("../Figures/Supplemental_Figures/Supplemental_Figure_2/Supplemental_Figure_2_Raw.tiff",
     height = 5, width = 10, res = 700, units = "in")
print(grid.arrange(Sup_Fig_2_A_comb, Sup_Fig_2_B_comb, Sup_Fig_2_C_comb, 
                   ncol = 3))
dev.off()



#grid.newpage()
#grid.draw(legend)