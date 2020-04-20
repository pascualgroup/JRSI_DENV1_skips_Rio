

# ---- mif_code ----

# Header ------------------------------------------------------------------
## Name: MIF_run_Model_A_7.R
## Author: Rahul Subramanian
## Description: Runs parameter combinations
## on midway for profile from original param grid
## for SIR model with cosine function (Model A_7)

rm(list = ls())
ptm <- proc.time()

#Load Libraries
source("load_libraries_essential.R")
source("rahul_theme.R")
library(pomp2)

args = commandArgs(trailingOnly = TRUE)
#param_index = as.numeric(args[1]) +
# as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

profile_var = as.character(args[1])
print(profile_var)

model_name = as.character(args[2])
print(model_name)


#Load dengue case data
Rio_data_clean = read.csv(
  "../Generated_Data/Rio_DENV1_Data_2_25_years_clean.csv")
head(Rio_data_clean)
t0 = as.numeric(as.Date("1986/05/01") - as.Date("1986/01/01"))


#Declare Csnippets and data
source("Csnippet_SIR_cosine_model.R")


require(foreach)
require(doParallel)
require(deSolve)

#Core management
no_cores <- detectCores()
cat("no_cores = ", no_cores, "\n")
cl <- makeCluster(no_cores)
registerDoParallel(cl)


param_index = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
print("param_index")
print(param_index)


##load(param_grid)
pd = read.csv(
  file = paste0(
    "../Generated_Data/Profile_Combination_Lists/",
    model_name,
    "_Model/",
    profile_var,
    "_",
    model_name,
    "_profile_combination_list.csv"
  ),
  header = TRUE
)
head(pd)

midway_max_jobs = 50
group_size = nrow(pd) / midway_max_jobs
start_index = (param_index - 1) * group_size + 1
end_index = param_index * group_size
Num_mif_runs_per_start = 5
param_data_subset_act = pd[start_index:end_index, ]
param_data_subset =
  param_data_subset_act[rep(seq_len(nrow(param_data_subset_act)),
                            each = Num_mif_runs_per_start), ]

rw_sd_list_default = rw.sd(
  Beta_0 = ifelse(time <= 365 * 2.50, 0.02, 0),
  delta = ifelse(time <= 365 * 2.50, 0.02, 0),
  phi = ifelse(time <= 365 * 2.50, 0.02, 0),
  sigma_P = 0,
  sigma_M = 0.02,
  I_0 = ivp(0.2),
  R_0 = 0,
  epsilon = 0)





get_rwsd = function(profile_var) {
  if (profile_var == "I_0") {
    rw.sd = rw.sd(
      Beta_0 = ifelse(time <= 365 * 2.50, 0.02, 0),
      delta = ifelse(time <= 365 * 2.50, 0.02, 0),
      phi = ifelse(time <= 365 * 2.50, 0.02, 0),
      rho = 0.02,
      sigma_P = 0,
      sigma_M = 0.02,
      I_0 = ivp(0),
      R_0 = 0,
      epsilon = 0
    )
  } else{
    if (profile_var == "Beta_0") {
      rw.sd = rw.sd(
        Beta_0 = 0,
        delta = ifelse(time <= 365 * 2.50, 0.02, 0),
        phi = ifelse(time <= 365 * 2.50, 0.02, 0),
        rho = 0.02,
        sigma_P = 0,
        sigma_M = 0.02,
        I_0 = ivp(0.2),
        R_0 = 0,
        epsilon = 0
      )
    } else{
      if (profile_var == "delta") {
        rw.sd = rw.sd(
          Beta_0 = ifelse(time <= 365 * 2.50, 0.02, 0),
          delta = 0,
          phi = ifelse(time <= 365 * 2.50, 0.02, 0),
          rho = 0.02,
          sigma_P = 0,
          sigma_M = 0.02,
          I_0 = ivp(0.2),
          R_0 = 0,
          epsilon = 0
        )
      } else{
        if (profile_var == "phi") {
          rw.sd = rw.sd(
            Beta_0 = ifelse(time <= 365 * 2.50, 0.02, 0),
            delta = ifelse(time <= 365 * 2.50, 0.02, 0),
            phi = 0,
            rho = 0.02,
            sigma_P = 0,
            sigma_M = 0.02,
            I_0 = ivp(0.2),
            R_0 = 0,
            epsilon = 0
          )
        } else{
          if (profile_var == "rho") {
            rw.sd = rw.sd(
              Beta_0 = ifelse(time <= 365 * 2.50, 0.02, 0),
              delta = ifelse(time <= 365 * 2.50, 0.02, 0),
              phi = ifelse(time <= 365 * 2.50, 0.02, 0),
              rho = 0,
              sigma_P = 0,
              sigma_M = 0.02,
              I_0 = ivp(0.2),
              R_0 = 0,
              epsilon = 0
            )
          } else{
              if (profile_var == "sigma_P") {
                rw.sd = rw.sd(
                  Beta_0 = ifelse(time <= 365 * 2.50, 0.02, 0),
                  delta = ifelse(time <= 365 * 2.50, 0.02, 0),
                  phi = ifelse(time <= 365 * 2.50, 0.02, 0),
                  rho = 0.02,
                  sigma_P = 0,
                  sigma_M = 0.02,
                  I_0 = ivp(0.2),
                  R_0 = 0,
                  epsilon = 0
                )
              } else{
                if (profile_var == "sigma_M") {
                  rw.sd = rw.sd(
                    Beta_0 = ifelse(time <= 365 * 2.50, 0.02, 0),
                    delta = ifelse(time <= 365 * 2.50, 0.02, 0),
                    phi = ifelse(time <= 365 * 2.50, 0.02, 0),
                    rho = 0.02,
                    sigma_P = 0,
                    sigma_M = 0,
                    I_0 = ivp(0.2),
                    R_0 = 0,
                    epsilon = 0
                  )
                } else{
                  if (profile_var == "R_0") {
                    rw.sd = rw.sd(
                      Beta_0 = ifelse(time <= 365 * 2.50, 0.02, 0),
                      delta = ifelse(time <= 365 * 2.50, 0.02, 0),
                      phi = ifelse(time <= 365 * 2.50, 0.02, 0),
                      rho = 0.02,
                      sigma_P = 0,
                      sigma_M = 0.02,
                      I_0 = ivp(0.2),
                      R_0 = 0,
                      epsilon = 0
                    )
                  } else{
                    if (profile_var == "epsilon") {
                      rw.sd = rw.sd(
                        Beta_0 = ifelse(time <= 365 * 2.50, 0.02, 0),
                        delta = ifelse(time <= 365 * 2.50, 0.02, 0),
                        phi = ifelse(time <= 365 * 2.50, 0.02, 0),
                        rho = 0.02,
                        sigma_P = 0,
                        sigma_M = 0.02,
                        I_0 = ivp(0.2),
                        R_0 = 0,
                        epsilon = 0
                      )
                    } else{
                      if (profile_var == "gamma") {
                        rw.sd = rw.sd(
                          Beta_0 = ifelse(time <= 365 * 2.50, 0.02, 0),
                          delta = ifelse(time <= 365 * 2.50, 0.02, 0),
                          phi = 0.02,
                          rho = 0.02,
                          sigma_P = 0,
                          sigma_M = 0.02,
                          I_0 = ivp(0.2),
                          R_0 = 0,
                          epsilon = 0,
                          gamma = 0
                        )
                        }else{
                          stop(
                            "Profile var not specified in rwsd wrapper function")
                          
                        }
                      
                    }
                    
                  }
                  
                }
              }
              
              
            }
          
          
        
    
    }
  }
}
}


return(rw.sd)
}



rw.sd = get_rwsd(profile_var = profile_var)

detail_log = FALSE

if (detail_log == TRUE) {
  detailed_log_file_name = paste0(
    "../Generated_Data/Profiles/",
    model_name,
    "_Model/",
    profile_var,
    "_Profile/Detailed_Log/log_file_subset_",
    param_index,
    ".txt"
  )
  write(file = detailed_log_file_name,
        paste0("Log generated on ", Sys.time(), " \n"),
        append = FALSE)
}


mif_single_subset_data <-
  foreach(
    i = 1:nrow(param_data_subset),
    .combine = rbind,
    .packages = 'pomp2',
    .export = c(
      "rproc",
      "rmeas",
      "dmeas",
      "init",
      "paramnames",
      "statenames",
      "obsnames",
      "param_data_subset",
      "par_trans",
      "acumvarnames",
      "covar"
    )
  )  %dopar%
  {
    mif_single_param_output <-
      get_MIF_final_params_and_pfilter_LL(
        data = Rio_data_clean,
        times = Rio_data_clean$times,
        t0 = t0,
        rproc = rproc,
        params = param_data_subset[i, ],
        paramnames = paramnames,
        statenames = statenames,
        obsnames = obsnames,
        dmeas = dmeas,
        accumvars = acumvarnames,
        init = init,
        rmeas = rmeas,
        par_trans = par_trans,
        Np = 10000,
        Nmif = 100,
        cooling.fraction.50 = 0.5,
        rw.sd = rw.sd,
        delta_time = 1,
        param_index = param_index,
        i = i,
        detail_log = detail_log,
        covar = covar
      )
    
  }

mif_single_subset_data <- as.data.frame(mif_single_subset_data)
stopCluster(cl)

last_col = ncol(mif_single_subset_data)
mif_single_subset_rel_data = mif_single_subset_data[, -last_col]
log_output = mif_single_subset_data[, last_col]
write.csv(
  mif_single_subset_rel_data,
  file = paste(
    "../Generated_Data/Profiles/",
    model_name,
    "_Model/",
    profile_var,
    "_Profile/Subset_Outputs/",
    profile_var,
    "_",
    model_name,
    "_Profile_subset_",
    param_index,
    ".csv",
    sep = ""
  ),
  row.names = FALSE,
  na = ""
)
if (detail_log == TRUE) {
  write(file = detailed_log_file_name, log_output, append = TRUE)
}


proc.time() - ptm
