### 
## Load data
rm(list = ls())
source("load_libraries_essential.R")
source("rahul_theme.R")
library(pomp)
t0 = as.numeric(as.Date("1986/05/01") - as.Date("1986/01/01"))
model_name_list = c("A_7")
model_label_list = factor(c("SIR Cosine No Immigration"))
model_label_list = factor(model_label_list, levels = c("SIR Cosine No Immigration"))

Csnippet_file_path_list = c("Csnippet_SIR_cosine_model.R")
Num_est_parameters_list = c(7)
data_file_path_list = c("../Generated_Data/Rio_DENV1_Data_2_25_years_clean.csv")

num_years_list = c(2.50)

model_ref_df = data.frame(
  model_name = model_name_list,
  model_label = model_label_list,
  Csnippet_file_path = Csnippet_file_path_list,
  Num_est_parameters = Num_est_parameters_list,
  data_file_path = data_file_path_list,
  num_years = num_years_list,
  stringsAsFactors = FALSE
)


model_index = 1
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


all_combo_data = read.csv(
  paste0(
    "../Generated_Data/Profiles/",
    model_name,
    "_Model/",
    model_name,
    "_Model_BP_top_2_LL_all_params_sim_cases_data.csv"
  )
)

bio_good_2_LL = read.csv(
  paste0(
    "../Generated_Data/Profiles/",
    model_name,
    "_Model/",
    model_name,
    "_Model_BP_top_2_LL_all_params_bio_good_2_LL.csv"
  )
)


ML_combo_num = which(bio_good_2_LL$LL == max(bio_good_2_LL$LL))

ML_output = filter(all_combo_data, combo_num == ML_combo_num)
ML_output = dplyr::select(
  ML_output,
  time = time,
  ML_median = sim_data_median,
  ML_high_Q = sim_data_high_Q,
  ML_low_Q = sim_data_low_Q
)
true_data = dplyr::select(Rio_clean_data, time = times,
                          Observed_Data = Y)



###

all_combo_melt_data = melt(all_combo_data, id.vars = c("time", "combo_num"))






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




fill_vec = c("Simulation Median \n  (all 2 LL combinations)" = "pink", "95% Simulation Quantiles \n (MLE)" = "skyblue", "95% Simulation Quantiles \n (all 2 LL combinations)" = "grey70")



p = ggplot(data = comp_data_melt) +
  geom_ribbon(aes(x = time/365, ymin = log(all_combo_low_Q_min),
                  ymax = log(all_combo_high_Q_max), fill = All_combo_Q_Rib_Col), inherit.aes = FALSE) +
  geom_ribbon(aes(x = time/365, ymin = log(ML_low_Q),
                  ymax = log(ML_high_Q), fill = ML_Q_Rib_Col),  inherit.aes = FALSE) +
  geom_ribbon(aes(x = time/365, ymin = log(all_combo_median_min),
                  ymax = log(all_combo_median_max), fill = All_combo_Med_Rib_Col), inherit.aes = FALSE) +
  geom_line(aes(x = time/365, y = log(value), color = variable)) +
  geom_point(aes(x = time/365, y = log(value), color = variable),
             size = 3) +
  rahul_theme +
  theme(legend.text = element_text(size = 12,
                                   face = "bold",
                                   color = "black")) +
  theme_white_background +
  scale_fill_manual(name = "Ribbon  Legend", values = fill_vec) +
  scale_color_manual(name = "Color  Legend", values = c("red","blue"),
                     labels =
                       c("Simulation Median \n (MLE)",
                         "Observed"))  +
  xlab("Years since Jan 1 1986")+
  ylab("log(Observed Monthly Cases) ") 
p
# pdf("../Figures/Supplemental_Figures/Supplemental_Figure_1/Sup_Fig_1A_zoom.pdf", height = 5, width = 10)
# print(p)
# dev.off()

pdf("../Figures/Supplemental_Figures/Supplemental_Figure_1/Sup_Fig_1.pdf", height = 5, width = 10)
p = p + rahul_man_figure_theme
print(p)
dev.off()
