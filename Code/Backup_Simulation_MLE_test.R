rm(list = ls())
model_name = "A_7"
Csnippet_file_path = "Csnippet_SIR_cosine_model.R"
Num_est_parameters = 7
data_file_path = "../Generated_Data/Rio_DENV1_Data_2_25_years_clean.csv"
num_years = 2.50
source("load_libraries_essential.R")
source("rahul_theme.R")
library(pomp)

#Load data
print(data_file_path)
Rio_data_clean = read.csv(file = data_file_path)
print(Rio_data_clean)


source(Csnippet_file_path, local = TRUE)

#Set t0
t0 = as.numeric(as.Date("1986/05/01") - as.Date("1986/01/01"))

#Load param combination directory
combined_profile_data = read.csv(
  file = paste0(
    "../Generated_Data/Profiles/",
    model_name,
    "_Model/combined_",
    model_name,
    "_profile_data_directory.csv"
  )
)
#head(combined_profile_data)
ML = max(combined_profile_data$LL, na.rm = TRUE)
MLE = filter(combined_profile_data, LL >= ML)
ML_params = dplyr::select(MLE, -Profile_Type)
##Simulation from ML
ML_params$r = 0
sim_data = simulate(nsim = 100,
                    seed = 12345,
                    times = Rio_data_clean$times,
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
                       true_data = Rio_data_first_three_years_only$Y)
comp_data = melt(comp_data, id.vars = c("time"))
N = ML_params[10]

dif_Y_over_N  =(sim_data_quant$Y$Q97.5/N)- (sim_data_quant$Y$Q2.5/N)
max(dif_Y_over_N)


comp_data = data.frame(time = sim_data_median_Y$time,
                       sim_data_median = sim_data_median_Y$Y,
                       sim_data_low_Q = sim_data_quant$Y$Q2.5,
                       sim_data_high_Q = sim_data_quant$Y$Q97.5,
                       true_data = Rio_data_clean$Y)

comp_data_melt = melt(comp_data, id.vars = c("time", "sim_data_low_Q",
                                             "sim_data_high_Q"))




p = ggplot(data = comp_data_melt) +
  geom_ribbon(aes(x = time/365, ymin = sim_data_low_Q,
                  ymax = sim_data_high_Q), fill = "grey70") +
  geom_line(aes(x = time/365, y = value, color = variable)) +
  geom_point(aes(x = time/365, y = value, color = variable)) +
  rahul_theme +
  theme_white_background +
  median_legend_lab +
  xlab("Years since Jan 1 1986")+
  ylab("Observed Monthly Cases")
p

p = ggplot(data = comp_data_melt) +
  geom_ribbon(aes(x = time/365, ymin = log(sim_data_low_Q),
                  ymax = log(sim_data_high_Q)), fill = "grey70") +
  geom_line(aes(x = time/365, y = log(value), color = variable)) +
  geom_point(aes(x = time/365, y = log(value), color = variable)) +
  rahul_theme +
  theme_white_background +
  median_legend_lab +
  xlab("Years since Jan 1 1986")+
  ylab("log(Observed Monthly Cases)")
p