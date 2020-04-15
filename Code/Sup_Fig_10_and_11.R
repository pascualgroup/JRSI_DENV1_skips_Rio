rm(list =ls())
source("load_libraries_essential.R")
source("rahul_theme.R")
library(stringr)
load("../Generated_Data/Skip_Data/nCritics.Rdata")
head(nCritics)
row.names(nCritics)
colnames(nCritics)
skip_raw_data = as.numeric(as.character(nCritics))
skip_data = as.data.frame(as.matrix(nCritics))

dim(nCritics) #18 (Reporitng rate) x 11 (delta) x 491 (R_0 value)
# Row name: Reporting rate (18 from 1% to 50%)



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
A_7_gamma_2_LL$nearest_skip_R_naught_index = NA
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
                                        "R[0]" = nearest_skip_R_naught,
                                        skips,
                                        rho = nearest_skip_rho,
                                        delta = nearest_skip_delta)
min(A_7_gamma_2_LL$skips, na.rm = TRUE)

relevant_skip_plot_data_melt = melt(
  relevant_skip_plot_data, id.vars = c("skips"))
S10_plot = ggplot(data = relevant_skip_plot_data_melt,
           aes(x = value, y = skips)) + geom_point() +
  facet_wrap(~variable, ncol = 4, scales= "free", labeller = label_parsed,
             strip.position = "bottom") + 
  rahul_man_figure_theme +theme_white_background +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    strip.placement = "outside"
  ) + 
  theme(legend.position = "None") +
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(face = "plain")) +
  ylab(expression(paste("Number of skips (", n[c], ")"))) +
  xlab("Recovery Rate     Reproductive Number     Reporting Rate   Seasonality Amplitude")
S10_plot
pdf("../Figures/Supplemental_Figures/Supplemental_Figure_10/Sup_Fig_10.pdf",
    height = 5, width = 10)
print(S10_plot)
dev.off()

S11_plot_data = A_7_gamma_2_LL %>%
  dplyr::select("R[0]" = nearest_skip_R_naught,
                              "sigma[P]" = sigma_P,
                              rho = nearest_skip_rho)
S11_plot_data_melt = S11_plot_data %>%
  melt(id.vars = c("rho"))
  
S11_plot = ggplot(data = S11_plot_data_melt,
                  aes(x = rho, y = value)) + geom_point() +
  facet_wrap(~variable, ncol = 1, scales= "free", labeller = label_parsed,
             strip.position = "left") + 
  rahul_man_figure_theme +theme_white_background +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    strip.placement = "outside"
  ) + 
  theme(legend.position = "None") +
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(face = "plain")) +
  xlab(expression(paste("Reporting Rate ", rho, ""))) +
  ylab("    Process Noise                      Reproductive Number")
S11_plot


pdf("../Figures/Supplemental_Figures/Supplemental_Figure_11/Sup_Fig_11.pdf")
S11_plot
dev.off()
