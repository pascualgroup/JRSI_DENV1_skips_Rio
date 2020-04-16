
rm(list = ls())
source("load_libraries_essential.R")
source("rahul_theme.R")
library(stringr)
library(gridExtra)
library(zoo)
load("../Generated_Data/Skip_Data/nCritics.Rdata")
head(nCritics)
row.names(nCritics)
colnames(nCritics)
skip_raw_data = as.numeric(as.character(nCritics))
skip_data = as.data.frame(as.matrix(nCritics))

dim(nCritics) #18 (Reporitng rate) x 11 (delta) x 491 (R_0 value)
# Row name: Reporting rate (18 from 1% to 50%)


#reporting_rates = strsplit(reporting_rate, "%")
rep_rate_header = str_split(row.names(nCritics), pattern = "%", n = Inf,
                            simplify = TRUE)
delta_col_header = str_split(colnames(nCritics), pattern = "_", n = Inf,
                             simplify = TRUE)
reporting_rate = as.numeric(rep_rate_header[,2])/100
delta_val = as.numeric(delta_col_header[,2])





R_0_col_header = str_split(names(nCritics[1,1,]), pattern = "_", n = Inf,
                           simplify = TRUE)
R_0_skip_val = as.numeric(R_0_col_header[,2])


#only_delta_07$Type = "Simulated"
##### Load data from old figure 8


#Load bio_good LL

model_name = "A_7"


## R_naught_act_data
profile_data_with_R_naught_act = read.csv(file = paste0("../Generated_Data/Profiles/", model_name,
                                                        "_Model/combined_", model_name, "_profile_data_directory_with_mean_R_0.csv"))

head(profile_data_with_R_naught_act)
MLE_with_R_naught_act = filter(profile_data_with_R_naught_act, LL == max(LL))
bio_good_2_LL_with_R_naught = read.csv(file = paste0("../Generated_Data/Profiles/", model_name,
                                                     "_Model/combined_", model_name, "_bio_good_2_LL_param_list.csv"))
A_7_MLE_R_naught_act = MLE_with_R_naught_act$R_naught
A_7_min_R_naught_act = min(bio_good_2_LL_with_R_naught$R_naught)
A_7_max_R_naught_act = max(bio_good_2_LL_with_R_naught$R_naught)

A_7_bio_good_2_LL = read.csv(paste0("../Generated_Data/Profiles/", model_name, "_Model/",model_name, "_Model_BP_top_2_LL_all_params_bio_good_2_LL.csv"))

A_7_bio_good_2_LL$R_naught_theo = A_7_bio_good_2_LL$Beta_0/(A_7_bio_good_2_LL$gamma + A_7_bio_good_2_LL$mu_H)

head(A_7_bio_good_2_LL)
A_7_bio_good_2_LL$nearest_skip_rho = 0
A_7_bio_good_2_LL$nearest_skip_R_naught = 0
A_7_bio_good_2_LL$nearest_skip_delta = 0
A_7_bio_good_2_LL$skips = -1
#single_param_data$nearest_skip_R_naught_index = NA
A_7_bio_good_2_LL$nearest_skip_delta_index = NA
A_7_bio_good_2_LL$nearest_skip_rho_index = NA
for(param_index in seq(1, nrow(A_7_bio_good_2_LL))){
  load("../Generated_Data/Skip_Data/nCritics_detailedRepRate_From2to5.Rdata")
  head(nCritics_detailedRepRate_From2to5)
  row.names(nCritics_detailedRepRate_From2to5)
  colnames(nCritics_detailedRepRate_From2to5)
  
  dim(nCritics) #18 (Reporitng rate) x 11 (delta) x 491 (R_0 value)
  # Row name: Reporting rate (18 from 1% to 50%)
  
  
  rep_rate_header_det = str_split(row.names(
    nCritics_detailedRepRate_From2to5), pattern = "%", n = Inf,
    simplify = TRUE)
  delta_col_header_det = str_split(colnames(
    nCritics_detailedRepRate_From2to5), pattern = "_", n = Inf,
    simplify = TRUE)
  reporting_rate_det = as.numeric(rep_rate_header_det[,2])/100
  delta_val_det = as.numeric(delta_col_header_det[,2])
  
  
  R_0_col_header_det = str_split(names(
    nCritics_detailedRepRate_From2to5[1,1,]), pattern = "_", n = Inf,
    simplify = TRUE)
  R_0_skip_val_det = as.numeric(R_0_col_header_det[,2])
  
  
  #single_param_data = A_7_bio_good_2_LL[param_index,]
  #Get R_naught ref on skip plot
  A_7_bio_good_2_LL$nearest_skip_R_naught_index[param_index] =
    which.min(abs(R_0_skip_val_det -
                    A_7_bio_good_2_LL$R_naught_theo[param_index] ))
  A_7_bio_good_2_LL$nearest_skip_R_naught[param_index] =
    R_0_skip_val_det[
      A_7_bio_good_2_LL$nearest_skip_R_naught_index[param_index]]
  #Get rho ref on skip plot
  
  A_7_bio_good_2_LL$nearest_skip_rho_index[param_index] =
    which.min(abs(reporting_rate_det -
                    A_7_bio_good_2_LL$rho[param_index] ))
  A_7_bio_good_2_LL$nearest_skip_rho[param_index] =
    reporting_rate_det[
      A_7_bio_good_2_LL$nearest_skip_rho_index[param_index]]
  
  #Get delta ref on skip plot
  A_7_bio_good_2_LL$nearest_skip_delta_index[param_index] =
    which.min(abs(delta_val_det -A_7_bio_good_2_LL$delta[param_index] ))
  A_7_bio_good_2_LL$nearest_skip_delta[param_index] =
    delta_val[
      A_7_bio_good_2_LL$nearest_skip_delta_index[param_index]]
  A_7_bio_good_2_LL$skips[param_index] =
    nCritics_detailedRepRate_From2to5[
      A_7_bio_good_2_LL$nearest_skip_rho_index[param_index],
      A_7_bio_good_2_LL$nearest_skip_delta_index[param_index],
      A_7_bio_good_2_LL$nearest_skip_R_naught_index[param_index]]
}

p = ggplot(data = A_7_bio_good_2_LL, aes(x = R_naught_theo,
                                         y = skips)) + geom_point() +
  rahul_theme
p

relevant_skip_plot_data = dplyr::select(A_7_bio_good_2_LL,
                                        "R[0]" = nearest_skip_R_naught,
                                        skips,
                                        rho = nearest_skip_rho,
                                        delta = nearest_skip_delta)
min(A_7_bio_good_2_LL$skips, na.rm = TRUE)

relevant_skip_plot_data_melt = melt(
  relevant_skip_plot_data, id.vars = c("skips"))
relevant_skip_plot_data_melt$value = signif(
  relevant_skip_plot_data_melt$value, digits = 3)
relevant_skip_plot_data_melt$skip_category = cut(
  relevant_skip_plot_data_melt$skips,breaks = c(0,1,100,101),
  include.lowest = TRUE)

relevant_skip_plot_data$r0 =
  relevant_skip_plot_data$`R[0]`


relevant_skip_plot_data$rho = as.factor(as.character(relevant_skip_plot_data$rho))


### Supplemental Figure S15

Sup_Fig_S15 = ggplot() +
  geom_point(data = relevant_skip_plot_data,
             aes(x = r0, y = skips),
             color = 'red', size = 4,
             shape =  "cross") +
  rahul_theme +
  theme_white_background + scale_color_viridis_d(direction = -1)+
  labs(x = expression(R[0])) +
  labs(color = expression(rho))+
  labs(y = expression(paste("Number of skips (", n[c], ")")))
  rahul_man_figure_theme 
Sup_Fig_S15
pdf("../Figures/Supplemental_Figures/Supplemental_Figure_15/Supplemental_Figure_S_15.pdf",
    height = 5, width = 10)
print(Sup_Fig_S15)
dev.off()