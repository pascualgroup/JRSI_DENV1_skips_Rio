rm(list =ls())
source("load_libraries_essential.R")
source("rahul_theme.R")
library(stringr)
load("../Down_Data/Skip_Data/nCritics.Rdata")
head(nCritics)
row.names(nCritics)
colnames(nCritics)
skip_raw_data = as.numeric(as.character(nCritics))
skip_data = as.data.frame(as.matrix(nCritics))

dim(nCritics) #18 (Reporitng rate) x 11 (delta) x 491 (R_0 value)
# Row name: Reporting rate (18 from 1% to 50%)


reporting_rates = strsplit(reporting_rate, "%")
reporting_rates = as
rep_rate_header = str_split(row.names(nCritics), pattern = "%", n = Inf,
              simplify = TRUE)
delta_col_header = str_split(colnames(nCritics), pattern = "_", n = Inf,
                             simplify = TRUE)
reporting_rate = as.numeric(rep_rate_header[,2])/100
delta_val = as.numeric(delta_col_header[,2])



R_0_col_header = str_split(names(nCritics[1,1,]), pattern = "_", n = Inf,
                             simplify = TRUE)
R_0_skip_val = as.numeric(R_0_col_header[,2])

#Load bio_good LL

model_name = "A_7"


## R_naught_act_data
profile_data_with_R_naught_act = read.csv(file = paste0("../Generated_Data/Profiles/", model_name,
                                                        "_Model/combined_", model_name, "_profile_data_directory_with_mean_R_0.csv"))

head(profile_data_with_R_naught_act)
MLE_with_R_naught_act = filter(profile_data_with_R_naught_act, LL == max(LL))

profile_var = "gamma"
#Load results
profile_data = read.csv(file = paste0("../Generated_Data/Profiles/", model_name, "_Model/", profile_var, "_Profile/",
                                      profile_var, "_", model_name, "_profile_combined_data.csv"))
#head(profile_data)

na_data = filter(profile_data, is.na(LL) == TRUE)
print(paste("There are ", nrow(na_data), " entries with NA likelihoods"))

profile_data_clean = na.omit(profile_data)


A_7_gamma_2_LL = filter(profile_data_clean, LL >= max(profile_data_clean$LL) - 2)

A_7_gamma_2_LL$R_naught_theo = A_7_gamma_2_LL$Beta_0/(A_7_gamma_2_LL$gamma + A_7_gamma_2_LL$mu_H)



A_7_gamma_2_LL$nearest_skip_rho = 0
A_7_gamma_2_LL$nearest_skip_R_naught = 0
A_7_gamma_2_LL$nearest_skip_delta = 0
A_7_gamma_2_LL$skips = -1
single_param_data$nearest_skip_R_naught_index = NA
A_7_gamma_2_LL$nearest_skip_delta_index = NA
A_7_gamma_2_LL$nearest_skip_rho_index = NA
for(param_index in seq(1, nrow(A_7_gamma_2_LL))){
  #Get R_naught ref on skip plot
  A_7_gamma_2_LL$nearest_skip_R_naught_index[param_index] =
    which.min(abs(R_0_skip_val -
                    A_7_gamma_2_LL$R_naught_theo[param_index] ))
  A_7_gamma_2_LL$nearest_skip_R_naught[param_index] =
    R_0_skip_val[
      A_7_gamma_2_LL$nearest_skip_R_naught_index[param_index]]
  #Get rho ref on skip plot
  
  A_7_gamma_2_LL$nearest_skip_rho_index[param_index] = 
    which.min(abs(reporting_rate -
                    A_7_gamma_2_LL$rho[param_index] ))
  A_7_gamma_2_LL$nearest_skip_rho[param_index] =
    reporting_rate[
      A_7_gamma_2_LL$nearest_skip_rho_index[param_index]]
  
  #Get delta ref on skip plot
  A_7_gamma_2_LL$nearest_skip_delta_index[param_index] =
    which.min(abs(delta_val -A_7_gamma_2_LL$delta[param_index] ))
  A_7_gamma_2_LL$nearest_skip_delta[param_index] =
    delta_val[
      A_7_gamma_2_LL$nearest_skip_delta_index[param_index]]
  A_7_gamma_2_LL$skips[param_index] =
    nCritics[A_7_gamma_2_LL$nearest_skip_rho_index[param_index],
             A_7_gamma_2_LL$nearest_skip_delta_index[param_index],
             A_7_gamma_2_LL$nearest_skip_R_naught_index[param_index]]
}

p = ggplot(data = A_7_gamma_2_LL, aes(x = R_naught_theo,
                                         y = skips)) + geom_point() +
  rahul_theme
p

relevant_skip_plot_data = dplyr::select(A_7_gamma_2_LL,
                                        gamma = gamma,
                                        R_naught = nearest_skip_R_naught,
                                        skips,
                                        rho = nearest_skip_rho,
                                        delta = nearest_skip_delta)
min(A_7_gamma_2_LL$skips, na.rm = TRUE)

relevant_skip_plot_data_melt = melt(
  relevant_skip_plot_data, id.vars = c("skips"))
p = ggplot(data = relevant_skip_plot_data_melt,
           aes(x = value, y = skips)) + geom_point() +
  facet_wrap(~variable, ncol = 4, scales= "free_x") + rahul_theme +
  xlab("Parameter") + ylab("Number of skips \n (Deterministic) ")
p
pdf("../Figures/Supplemental_Figures/Supplemental_Figure_11/Sup_Fig_11.pdf")
print(p)
dev.off()

p = ggplot(data = A_7_gamma_2_LL, aes(x= rho, y = LL)) + geom_point()
p
p = ggplot(data = A_7_gamma_2_LL, aes(x= rho, y = gamma)) + geom_point()
p
p = ggplot(data = A_7_gamma_2_LL, aes(x= rho, y = R_naught_theo,
                                      color = gamma)) +
  scale_color_viridis_c()+
  geom_point() + rahul_theme
p
pdf("../Figures/Supplemental_Figures/Supplemental_Figure_12/Sup_Fig_12.pdf")
p
dev.off()

p = ggplot(data = A_7_gamma_2_LL, aes(x = rho, y = sigma_P)) +
  geom_point() + rahul_theme
p
pdf("../Figures/Supplemental_Figures/Supplemental_Figure_13/Sup_Fig_13.pdf")
p
dev.off()
p = ggplot(data = A_7_gamma_2_LL, aes(x = R_naught_theo, y = sigma_P)) + geom_point() +
  rahul_theme
p
pdf("../Figures/Supplemental_Figures/Supplemental_Figure_14/Sup_Fig_14.pdf")
p
dev.off()