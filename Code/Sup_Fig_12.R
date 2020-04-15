rm(list = ls())
model_name = "A_7"
Csnippet_file_path = "Csnippet_SIR_cosine_model.R"
Num_est_parameters = 7
data_file_path = "../Generated_Data/Rio_DENV1_Data_2_25_years_clean.csv"
num_years = 2.50
source("load_libraries_essential.R")
source("rahul_theme.R")
library(pomp2)

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
top_2_LL = filter(combined_profile_data, LL >= ML-2)
big_sigma_P_param = filter(top_2_LL, sigma_P >=max(top_2_LL$sigma_P))
max_LL_with_big_sigma_P_param = filter(big_sigma_P_param, LL >=max(big_sigma_P_param$LL))

MLE

ML_params = dplyr::select(MLE,-one_of("seed", "LL", "Profile_Type"))


ptm <- proc.time()
library(doRNG)
Rio_data_first_two_and_half_years_only = filter(Rio_data_clean, times <= 365 *
                                                  num_years)




registerDoRNG(123456)

foreach(
  i = 1:10,
  .packages = 'pomp2',
  .export = c(
    "rproc",
    "rmeas",
    "dmeas",
    "mif_sim_data",
    "init",
    "paramnames",
    "statenames",
    "obsnames",
    "params",
    "par_trans",
    "acumvarnames",
    "covar"
  )
) %dopar% {
  pfilter(
    data = Rio_data_first_two_and_half_years_only,
    times = Rio_data_first_two_and_half_years_only$times,
    t0 = t0,
    rprocess = euler(rproc, delta.t = 1),
    params = ML_params,
    paramnames = paramnames,
    statenames = statenames,
    obsnames = obsnames,
    dmeas = dmeas,
    accumvars = acumvarnames,
    covar = covar,
    rinit = init,
    rmeas = rmeas,
    partrans = par_trans,
    format = "data.frame",
    Np = 1000,
    pred.mean = TRUE,
    pred.var = TRUE,
    filter.mean = TRUE,
    filter.traj = TRUE
  )
} -> pfilter_ML_output_two_and_a_half_years
proc.time() - ptm

(mle_likelihood_baseline_two_and_half_years <-
    logmeanexp(
      sapply(pfilter_ML_output_two_and_a_half_years, logLik),
      se = TRUE
    ))

##Get pfilter mean for each distribution
pfilter_mean_cases = data.frame(matrix(nrow = 0, ncol = nrow(Rio_data_first_two_and_half_years_only)))
colnames(pfilter_mean_cases) = Rio_data_first_two_and_half_years_only$times

pfilter_var_cases = data.frame(matrix(nrow = 0, ncol = nrow(Rio_data_first_two_and_half_years_only)))
colnames(pfilter_var_cases) = Rio_data_first_two_and_half_years_only$times
pfilter_resid_cases = data.frame(matrix(nrow = 0, ncol = nrow(Rio_data_first_two_and_half_years_only)))
colnames(pfilter_resid_cases) = Rio_data_first_two_and_half_years_only$times
for(i in seq(from = 1, to = 10, by = 1)){
  single_pfilter_run = pfilter_ML_output_two_and_a_half_years[[i]]
  single_pfilter_filter_mean = filter.mean(single_pfilter_run)
  single_pfilter_pred_var = pred.var(single_pfilter_run)
  
  single_pfilter_filter_mean_df = as.data.frame(single_pfilter_filter_mean,
                                                row.names = row.names(single_pfilter_filter_mean))
  single_pfilter_pred_var_df = as.data.frame(single_pfilter_pred_var,
                                                row.names = row.names(single_pfilter_pred_var))
  single_pfilter_filter_mean_df$variable = row.names(single_pfilter_filter_mean)
  single_pfilter_pred_var_df$variable = row.names(single_pfilter_pred_var)
  
  single_pfilter_filter_mean_cases = filter(single_pfilter_filter_mean_df,
                                            variable == "C")
  single_pfilter_pred_var_cases = filter(single_pfilter_pred_var_df,
                                            variable == "C")
  single_pfilter_filter_mean_cases = dplyr::select(single_pfilter_filter_mean_cases, -variable)
  single_pfilter_pred_var_cases = dplyr::select(single_pfilter_pred_var_cases, -variable)
  
  single_pfilter_filter_resid_cases = Rio_data_first_two_and_half_years_only$Y - single_pfilter_filter_mean_cases
  pfilter_mean_cases = rbind(pfilter_mean_cases, single_pfilter_filter_mean_cases)
  pfilter_var_cases = rbind(pfilter_var_cases, single_pfilter_pred_var_cases)
  
  pfilter_resid_cases = rbind(pfilter_resid_cases, single_pfilter_filter_resid_cases)
  single_pfilter_filter_cond_log_lik = cond.logLik(single_pfilter_run)
  
}
mean_of_residuals_accross_all_pfilter_runs = colMeans(pfilter_resid_cases)
MLE_resid_plot = data.frame(time = Rio_data_first_two_and_half_years_only$times,
                            MLE_resid_mean = mean_of_residuals_accross_all_pfilter_runs,
                            log_data = log(Rio_data_first_two_and_half_years_only$Y))
MLE_resid_plot_melt = melt(MLE_resid_plot, id.vars = ("time"))
p = ggplot(data = MLE_resid_plot_melt, aes(x = time, y = value, color = variable)) +
  geom_point() + geom_line() + rahul_theme + facet_wrap(~variable,ncol = 1, scales = "free_y")
p
third_year_melt_data = filter(MLE_resid_plot_melt, time >= 365*2)
p = ggplot(data = third_year_melt_data, aes(x = time, y = value, color = variable)) +
  geom_point() + geom_line() + rahul_theme + facet_wrap(~variable,ncol = 1, scales = "free_y")
p    

MLE_resid_plot = data.frame(time = Rio_data_first_two_and_half_years_only$times,
                            MLE_resid_mean = mean_of_residuals_accross_all_pfilter_runs/Rio_data_first_two_and_half_years_only$Y,
                            log_data = log(Rio_data_first_two_and_half_years_only$Y))
MLE_resid_plot_melt = melt(MLE_resid_plot, id.vars = ("time"))
p = ggplot(data = MLE_resid_plot_melt, aes(x = time, y = value, color = variable)) +
  geom_point() + geom_line() + rahul_theme + facet_wrap(~variable,ncol = 1, scales = "free_y")
p
third_year_melt_data = filter(MLE_resid_plot_melt, time >= 365*2)
p = ggplot(data = third_year_melt_data, aes(x = time, y = value, color = variable)) +
  geom_point() + geom_line() + rahul_theme + facet_wrap(~variable,ncol = 1, scales = "free_y")
p    

mean_of_filter_means_accross_all_pfilter_runs = colMeans(pfilter_mean_cases)
mean_of_pred_var_accross_all_pfilter_runs = colMeans(pfilter_var_cases)


MLE_resid_plot = data.frame(time = Rio_data_first_two_and_half_years_only$times,
                            MLE_filter_mean = mean_of_filter_means_accross_all_pfilter_runs,
                            log_data = Rio_data_first_two_and_half_years_only$Y)
MLE_resid_plot_melt = melt(MLE_resid_plot, id.vars = ("time"))
p = ggplot(data = MLE_resid_plot_melt, aes(x = time, y = value, color = variable)) +
  geom_point() + geom_line() + rahul_theme + facet_wrap(~variable,ncol = 1, scales = "free_y")
p
third_year_melt_data = filter(MLE_resid_plot_melt, time >= 365*2)
p = ggplot(data = third_year_melt_data, aes(x = time, y = value, color = variable)) +
  geom_point() + geom_line() + rahul_theme + facet_wrap(~variable,ncol = 1, scales = "free_y")
p    
p = ggplot(data = third_year_melt_data, aes(x = time, y = value, color = variable)) +
  geom_point() + geom_line() + rahul_theme
p    

filter.mean(pfilter_ML_output_two_and_a_half_years[[1]])

# Big simpa P combination

top_2_LL = filter(combined_profile_data, LL >= ML-2)
big_sigma_P_param = filter(top_2_LL, sigma_P >=max(top_2_LL$sigma_P))
max_LL_with_big_sigma_P_param = filter(big_sigma_P_param, LL >=max(big_sigma_P_param$LL))

big_Beta_0 = filter(top_2_LL, Beta_0 >=max(top_2_LL$Beta_0))
MLE

ML_params = dplyr::select(MLE,-one_of("seed", "LL", "Profile_Type"))
Big_sigma_P_param = dplyr::select(max_LL_with_big_sigma_P_param,-one_of("seed", "LL", "Profile_Type"))


ptm <- proc.time()
library(doRNG)
Rio_data_first_two_and_half_years_only = filter(Rio_data_clean, times <= 365 *
                                                  num_years)




registerDoRNG(123456)

foreach(
  i = 1:10,
  .packages = 'pomp2',
  .export = c(
    "rproc",
    "rmeas",
    "dmeas",
    "mif_sim_data",
    "init",
    "paramnames",
    "statenames",
    "obsnames",
    "params",
    "par_trans",
    "acumvarnames",
    "covar"
  )
) %dopar% {
  pfilter(
    data = Rio_data_first_two_and_half_years_only,
    times = Rio_data_first_two_and_half_years_only$times,
    t0 = t0,
    rprocess = euler(rproc, delta.t = 1),
    params = Big_sigma_P_param,
    paramnames = paramnames,
    statenames = statenames,
    obsnames = obsnames,
    dmeas = dmeas,
    accumvars = acumvarnames,
    covar = covar,
    rinit = init,
    rmeas = rmeas,
    partrans = par_trans,
    format = "data.frame",
    Np = 1000,
    pred.mean = TRUE,
    pred.var = TRUE,
    filter.mean = TRUE,
    filter.traj = TRUE
  )
} -> pfilter_Big_sigma_P_params_output_two_and_a_half_years
proc.time() - ptm

(mle_likelihood_baseline_two_and_half_years <-
    logmeanexp(
      sapply(pfilter_Big_sigma_P_params_output_two_and_a_half_years, logLik),
      se = TRUE
    ))

##Get pfilter mean for each distribution
big_sigma_pfilter_mean_cases = data.frame(matrix(nrow = 0, ncol = nrow(Rio_data_first_two_and_half_years_only)))
colnames(big_sigma_pfilter_mean_cases) = Rio_data_first_two_and_half_years_only$times
big_sigma_pfilter_var_cases = data.frame(matrix(nrow = 0, ncol = nrow(Rio_data_first_two_and_half_years_only)))
colnames(big_sigma_pfilter_var_cases) = Rio_data_first_two_and_half_years_only$times

big_sigma_pfilter_resid_cases = data.frame(matrix(nrow = 0, ncol = nrow(Rio_data_first_two_and_half_years_only)))
colnames(big_sigma_pfilter_resid_cases) = Rio_data_first_two_and_half_years_only$times
for(i in seq(from = 1, to = 10, by = 1)){
  single_pfilter_run = pfilter_Big_sigma_P_params_output_two_and_a_half_years[[i]]
  single_pfilter_filter_mean = filter.mean(single_pfilter_run)
  single_pfilter_pred_var = pred.var(single_pfilter_run)
  
  single_pfilter_filter_mean_df = as.data.frame(single_pfilter_filter_mean,
                                                row.names = row.names(single_pfilter_filter_mean))
  single_pfilter_pred_var_df = as.data.frame(single_pfilter_pred_var,
                                               row.names = row.names(single_pfilter_pred_var))
  single_pfilter_filter_mean_df$variable = row.names(single_pfilter_filter_mean)
  single_pfilter_pred_var_df$variable = row.names(single_pfilter_pred_var)
  
  single_pfilter_filter_mean_cases = filter(single_pfilter_filter_mean_df,
                                            variable == "C")
  single_pfilter_pred_var_cases = filter(single_pfilter_pred_var_df,
                                           variable == "C")
  
  single_pfilter_filter_mean_cases = dplyr::select(single_pfilter_filter_mean_cases, -variable)
  single_pfilter_pred_var_cases = dplyr::select(single_pfilter_pred_var_cases, -variable)
  
  single_pfilter_filter_resid_cases = Rio_data_first_two_and_half_years_only$Y - single_pfilter_filter_mean_cases
  big_sigma_pfilter_mean_cases = rbind(big_sigma_pfilter_mean_cases, single_pfilter_filter_mean_cases)
  big_sigma_ppred_var_cases = rbind(big_sigma_pfilter_mean_cases, single_pfilter_pred_var_cases)
  
  big_sigma_pfilter_resid_cases = rbind(big_sigma_pfilter_resid_cases, single_pfilter_filter_resid_cases)
  big_sigma_single_pfilter_filter_cond_log_lik = cond.logLik(single_pfilter_run)
  
}
big_sigma_mean_of_residuals_accross_all_pfilter_runs = colMeans(big_sigma_pfilter_resid_cases)
big_sigma_mean_of_filter_means_accross_all_pfilter_runs = colMeans(big_sigma_pfilter_mean_cases)
big_sigma_mean_of_pred_var_accross_all_pfilter_runs = colMeans(big_sigma_ppred_var_cases)

##
Combined_filter_mean_case_comp_plot = data.frame(time = Rio_data_first_two_and_half_years_only$times,
                            MLE = mean_of_filter_means_accross_all_pfilter_runs,
                            Big_sigma_P = big_sigma_mean_of_filter_means_accross_all_pfilter_runs,
                            log_data = Rio_data_first_two_and_half_years_only$Y)
Combined_pred_var_case_comp_plot = data.frame(time = Rio_data_first_two_and_half_years_only$times,
                                                 MLE = mean_of_pred_var_accross_all_pfilter_runs,
                                                 Big_sigma_P = big_sigma_mean_of_pred_var_accross_all_pfilter_runs)
Combined_filter_mean_case_comp_plot_melt = melt(Combined_filter_mean_case_comp_plot, id.vars = ("time"))
Combined_pred_var_case_comp_plot_melt = melt(Combined_pred_var_case_comp_plot, id.vars = ("time"))
colnames(Combined_filter_mean_case_comp_plot_melt) = c("time", "variable", "filter_mean")
colnames(Combined_pred_var_case_comp_plot_melt) = c("time", "variable", "pred_var")
combined_filter_mean_and_pred_var_df = join(Combined_filter_mean_case_comp_plot_melt,
                                            Combined_pred_var_case_comp_plot_melt)
third_year_melt_data = filter(Combined_filter_mean_case_comp_plot_melt, time >= 365*2)
third_year_combined_melt_data = filter(combined_filter_mean_and_pred_var_df, time >= 365*2)

p = ggplot(data = third_year_melt_data, aes(x = time, y = value, color = variable)) +
  geom_point() + geom_line() + rahul_theme + facet_wrap(~variable,ncol = 1, scales = "free_y")
p    
p = ggplot(data = third_year_melt_data, aes(x = time/365, y = value, color = variable)) +
  geom_point() + geom_line() + rahul_theme
p
p = ggplot(data = third_year_combined_melt_data, aes(x = time/365, y = filter_mean, color = variable)) +
  geom_point() + geom_line() + rahul_theme + xlab("Years since January 1, 1986")
p

pdf("../Figures/Supplemental_Figures/Supplemental_Figure_15/Supplemental_Figure_15.pdf")
print(p)
dev.off()
p = ggplot(data = third_year_combined_melt_data, aes(x = time/365, y = filter_mean, color = variable)) +
  geom_ribbon(aes(ymin = filter_mean-2*sqrt(pred_var),
                   ymax = filter_mean+2*sqrt(pred_var),
                   fill = variable),
              alpha = 0.25) +
  geom_point() + geom_line() + rahul_theme + xlab("Years since January 1, 1986")
p
pdf("../Figures/Supplemental_Figures/Supplemental_Figure_16/Supplemental_Figure_16.pdf")
print(p)
dev.off()
second_year_melt_data = filter(Combined_filter_mean_case_comp_plot_melt, time >= 365*1)
second_year_melt_data = filter(second_year_melt_data, time <= 365*2)
p = ggplot(data = second_year_melt_data, aes(x = time, y = value, color = variable)) +
  geom_point() + geom_line() + rahul_theme
p    

second_year_combined_melt_data = filter(combined_filter_mean_and_pred_var_df, time >= 365*1)
second_year_combined_melt_data = filter(second_year_combined_melt_data, time <= 365*2)

p = ggplot(data = second_year_combined_melt_data, aes(x = time/365, y = filter_mean, color = variable)) +
  geom_ribbon(aes(ymin = filter_mean-2*sqrt(pred_var),
                  ymax = filter_mean+2*sqrt(pred_var),
                  fill = variable),
              alpha = 0.25) +
  geom_point() + geom_line() + rahul_theme + xlab("Years since January 1, 1986")
p
pdf("../Figures/Supplemental_Figures/Supplemental_Figure_17/Supplemental_Figure_17.pdf")
print(p)
dev.off()