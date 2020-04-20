# ---- generate_profile_combinataions ----


# Header ------------------------------------------------------------------
## Name: generate_profile_combinations_SIR_Cosine.R
## Author: Rahul Subramanian
## Description: Creates 30*40-combination list for given by
## profile_var as 1st command line argument
rm(list = ls())

ptm <- proc.time()

#Load Libraries
source("load_libraries_essential.R")
source("rahul_theme.R")
library(pomp2)

#profile_var = "I_0"
#model_name = "SEIR_Spline_2_Year"

args = commandArgs(trailingOnly=TRUE)

profile_var = as.character(args[1])
print(profile_var)

model_name = as.character(args[2])
print(model_name)

city_name = as.character(args[3])

serotype_name = as.character(args[4])

R_Init_status = as.character(args[5])

Immigration_status = as.character(args[6])

Duration_status = as.character(args[7])

city_specific_param_boundaries = data.frame(City = c("Rio", "Rio", "Fortaleza",
                                                     "Rio","Rio", "Rio",
                                                     "Rio", "Rio","Rio"),
                                            Serotype = c("DENV1", "DENV4",
                                                         "DENV4", "DENV1", 
                                                         "DENV1",  "DENV1",
                                                         "DENV1", "DENV1",
                                                         "DENV1"),
                                            R_Init_Status = c("Fix_R_Init",
                                                              "Fix_R_Init",
                                                              "Fix_R_Init",
                                                              "Fit_R_Init",
                                                              "Fit_R_Init",
                                                              "Fit_R_Init",
                                                              "Fix_R_Init",
                                                              "Fix_R_Init",
                                                              "Fix_R_Init"),
                                            Immigration = c("No_Immigration",
                                                            "No_Immigration",
                                                            "No_Immigration",
                                                            "No_Immigration",
                                                            "Immigration",
                                                            "Immigration",
                                                            "Immigration",
                                                            "No_Immigration",
                                                            "No_Immigration"),
                                            Duration_Params = c("Fit_Duration",
                                                                "Fit_Duration",
                                                            "Fit_Duration",
                                                            "Fit_Duration",
                                                            "Fit_Duration",
                                                            "Fix_Duration",
                                                            "Fix_Duration",
                                                            "Fix_Duration",
                                                            "Profile_Duration"),
                                            rho_upper = c(0.001, 0.15, 0.001,
                                                          0.001,0.001,  0.001,
                                                          0.001,  0.001,0.001),
                                            rho_lower = c(0.15, 0.15, 0.15, 0.15,
                                                          0.15, 0.15, 0.15, 0.15,
                                                          0.15),
                                            N_0_upper = c(5.301405e+06,
                                                          6.320446e+06,
                                                          2.452185e+06,
                                                          5.301405e+06,
                                                          5.301405e+06,
                                                          5.301405e+06,
                                                          5.301405e+06,
                                                          5.281842e+06,
                                                          5.281842e+06),
                                            N_0_lower = c(5.301405e+06,
                                                          6.320446e+06,
                                                          2.452185e+06,
                                                          5.301405e+06, 
                                                          5.301405e+06,
                                                          5.301405e+06,
                                                          5.301405e+06,
                                                          5.281842e+06,
                                                          5.281842e+06),
                                            R_0_lower = c(0, 0, 0, 0,
                                                          0, 0, 0, 0,
                                                          0),
                                            R_0_upper = c(0, 0, 0, 5.101405e+06,
                                                          5.101405e+06,
                                                          5.101405e+06, 0, 0,
                                                          0),
                                            Beta_0_lower = c(-3, -2, -4, -3,
                                                             -5.5, -3,-3,0,
                                                             0),
                                            Beta_0_upper = c(5, 1.75, 2, 7.5,
                                                             7.5, 7.5, 7.5, 0.25,
                                                             0.25),
                                            delta_lower = c(-6, -4.5, -3.5, -8,
                                                            -7.5, -8, -8, 0,
                                                            0),
                                            delta_upper = c(3, 0, 1.5, 3,
                                                            5, 5, 5, 1,
                                                            1),
                                            phi_lower = c(-15, -8.0, -7.5, -18,
                                                          -15, -18, -18, 0,
                                                          0),
                                            phi_upper = c(6, 0.5, 1.25, 6,
                                                          6, 6, 6, pi,
                                                          pi),
                                            omega_lower = c(0, 0, 0, 0,
                                                            -4, 0, 0, (2*pi)/365,
                                                            (2*pi)/365),
                                            omega_upper = c(0, 0, 0, 0,
                                                            4, 0, 0, (2*pi)/365,
                                                            (2*pi)/365),
                                            epsilon_lower = c(0, 0, 0, 0,
                                                              0, 0, 0, 0,
                                                              0),
                                            epsilon_upper = c(0, 0, 0, 0,
                                                              0.2, 0.2, 0.2, 0,
                                                              0),
                                            I_0_lower = c(1, 1, 1, 1,
                                                          1, 1, 1, 1,
                                                          1),
                                            I_0_upper = c(1.000000e+07,
                                                          2.000000e+05,
                                                          2.000000e+05,
                                                          1.000000e+06,
                                                          1.000000e+06,
                                                          6.000000e+05,
                                                          6.000000e+05,
                                                          6.000000e+05,
                                                          6.000000e+05),
                                            sigma_M_lower= c(.001, .001,
                                                             .001, .001,
                                                             0, .0001,
                                                             .0001, .0001,
                                                             .0001),
                                            sigma_M_upper = c(1, .25, .25, 1,
                                                              1, 1, 1, 1,
                                                              1),
                                            gamma_lower = c(1/17, 1/17,
                                                            1/17, 1/17,
                                                            1/17, 1/17,
                                                            1/17, 1/17,
                                                            1/22),
                                            gamma_upper = c(1/4, 1/4,
                                                            1/4, 1/4,
                                                            1/4, 1/17,
                                                            1/17, 1/17,
                                                            1/2),
                                            sigma_P_lower = c(1.9e-4, 1.9e-4,
                                                              1.9e-4, 1.9e-4,
                                                              1.9e-4, 1.9e-4,
                                                              1.9e-4, 1.9e-4,
                                                              1.9e-4),
                                            sigma_P_upper = c(3.8e1, 3.8e1,
                                                              3.8e1, 3.8e1,
                                                              3.8e1, 3.8e1,
                                                              1, 1,
                                                              1))

city_specific_param_boundaries = filter(city_specific_param_boundaries,
                                        City == city_name)
city_specific_param_boundaries = filter(city_specific_param_boundaries,
                                        Serotype == serotype_name)
city_specific_param_boundaries = filter(city_specific_param_boundaries,
                                        R_Init_Status == R_Init_status)
city_specific_param_boundaries = filter(city_specific_param_boundaries,
                                        Immigration == Immigration_status)
city_specific_param_boundaries = filter(city_specific_param_boundaries,
                                        Duration_Params == Duration_status)

rho_upper = city_specific_param_boundaries$rho_upper
rho_lower = city_specific_param_boundaries$rho_lower
N_0_upper = city_specific_param_boundaries$N_0_upper
N_0_lower = city_specific_param_boundaries$N_0_lower
R_0_lower = city_specific_param_boundaries$R_0_lower
R_0_upper = city_specific_param_boundaries$R_0_upper

Beta_0_lower = city_specific_param_boundaries$Beta_0_lower
Beta_0_upper = city_specific_param_boundaries$Beta_0_upper
delta_lower = city_specific_param_boundaries$delta_lower
delta_upper = city_specific_param_boundaries$delta_upper
phi_lower = city_specific_param_boundaries$phi_lower
phi_upper = city_specific_param_boundaries$phi_upper
omega_lower = city_specific_param_boundaries$omega_lower
omega_upper = city_specific_param_boundaries$omega_upper
epsilon_lower = city_specific_param_boundaries$epsilon_lower
epsilon_upper = city_specific_param_boundaries$epsilon_upper

I_0_lower = city_specific_param_boundaries$I_0_lower
I_0_upper = city_specific_param_boundaries$I_0_upper

sigma_M_lower = city_specific_param_boundaries$sigma_M_lower
sigma_M_upper = city_specific_param_boundaries$sigma_M_upper

gamma_lower = city_specific_param_boundaries$gamma_lower
gamma_upper = city_specific_param_boundaries$gamma_upper

sigma_P_lower = city_specific_param_boundaries$sigma_P_lower
sigma_P_upper = city_specific_param_boundaries$sigma_P_upper



par_box_boundaries = rbind(
  c(gamma_lower, gamma_upper), # gamma 
  c(phi_lower,phi_upper), # phi
  c(sigma_P_lower, sigma_P_upper), # sigma_P
  c(sigma_M_lower,sigma_M_upper), # sigma_M
  c(rho_lower,rho_upper), # rho
  c(Beta_0_lower,Beta_0_upper), # Beta_0
  c(delta_lower, delta_upper), # delta
  c(3.680000e-05,3.680000e-05), # mu_H
  c(N_0_lower,N_0_upper), # N_0
  c(I_0_lower,I_0_upper), # I_0
  c(R_0_lower,R_0_upper), # R_0
  c(0,0), #C_0
  c(0,0), #r
  c(omega_lower, omega_upper), #omega
  c(epsilon_lower, epsilon_upper) #epsilon
)

par_box_boundaries = t(par_box_boundaries)
names <- c("gamma","phi","sigma_P","sigma_M","rho","Beta_0","delta",
           "mu_H","N_0","I_0","R_0","C_0", "r", "omega", "epsilon")
colnames(par_box_boundaries) = names
par_box_boundaries = as.data.frame(par_box_boundaries)
par_box_boundaries_clean = dplyr::select(par_box_boundaries,
                                         -one_of(profile_var) )
theta.t.lo = as.numeric(as.vector(par_box_boundaries_clean[1,]))
theta.t.hi = as.numeric(as.vector(par_box_boundaries_clean[2,]))
names(theta.t.lo) = colnames(par_box_boundaries_clean)
names(theta.t.hi) = colnames(par_box_boundaries_clean)

prof_var_boundaries = dplyr::select(par_box_boundaries, one_of(profile_var))
profileDesign(
  prof_var=seq(from=prof_var_boundaries[1,],
               to=prof_var_boundaries[2,],length=30),
  lower=theta.t.lo,upper=theta.t.hi,nprof=40
) -> pd
pd_col = colnames(pd)
colnames(pd) = c(profile_var, pd_col[2:length(pd_col)])


write.csv(pd, file = paste0("../Generated_Data/Profile_Combination_Lists/",
                            model_name,"_Model/", profile_var,"_",
                            model_name,
                            "_profile_combination_list.csv"),
     append = FALSE, row.names = FALSE)
proc.time() - ptm
