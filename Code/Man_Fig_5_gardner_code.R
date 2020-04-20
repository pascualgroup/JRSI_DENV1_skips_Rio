# ---- Fig_5_Cluster_Code ----


#Make one line of heat map for plot of
## re-emergence probability given spark size and time
#Here we give it a single time and iterate 
# over multiple spark sizes, param combinatinos (all
# BP params within 2LL of BP MLE), and simulations (N_sim = 100)
rm(list = ls())
source("load_libraries_essential.R")
library(zoo)
library(pomp)
source("rahul_theme.R")
args = commandArgs(trailingOnly = TRUE)

model_name = as.character(args[1])
#model_name = "A_3"
print(model_name)

Rio_data_clean = read.csv(
  "../Generated_Data/Rio_DENV1_Data_3_75_years_clean.csv")
head(Rio_data_clean)
Rio_clean_data = Rio_data_clean
t0 = as.numeric(as.Date("1986/05/01") -
                  as.Date("1986/01/01"))
load(file = "../Down_Data/denguerj1986-2017.RData")
Rio_city_DENV1_clean = data.frame(
  Y = as.matrix(dengue.ts),
  Date = as.Date(as.yearmon(time(dengue.ts))))
Rio_city_DENV1_clean = filter(
  Rio_city_DENV1_clean,
  Date >= "1986-05-01")

head(Rio_city_DENV1_clean)
Rio_city_DENV1_clean$Date =
  Rio_city_DENV1_clean$Date %m+% months(1)

head(Rio_city_DENV1_clean)
##Calculate Re-Emergence Probability

Population_Rio_2000 = 5857904 #Census
Population_Rio_1991 = 5480768# Census:
Two_hour_segments_in_year = 365 * 12
time_between_census_dates = 2000 * 365 - 1991 * 365
human_pop_growth_rate =
  (1 / time_between_census_dates) *
  log(Population_Rio_2000 / Population_Rio_1991)
human_pop_growth_rate

#Source Csnippets
source(file = "Csnippet_SIR_cosine_model.R")


all_combos = read.csv(
  paste0(
    "../Generated_Data/Profiles/",
    model_name,
    "_Model/combined_",
    model_name,
    "_profile_data_directory_with_mean_R_0.csv"
  )
)

MLE_params = filter(all_combos, LL == max(LL))

bio_good_2_LL = filter(
  all_combos, LL > max(all_combos$LL) - 2)

within_20_LL = filter(
  all_combos, LL > max(all_combos$LL) - 20)

param_index = as.numeric(
  Sys.getenv("MOAB_JOBARRAYINDEX"))
print("param_index")
print(param_index)
gardner_max_jobs = 500
group_size = ceiling(nrow(
  bio_good_2_LL) / gardner_max_jobs)
start_index = (param_index - 1) * group_size + 1
end_index = param_index * group_size
Num_mif_runs_per_start = 5
param_data_subset =
  bio_good_2_LL[start_index:end_index,]

start_date = as.Date("1986-01-01") +
  min(Rio_clean_data$times)
long_re_emergence_time_series_2 =
  seq.Date(from = start_date, by = "month", length = 600)

years_in_long_re_emergence_time_series_2 =
  year(long_re_emergence_time_series_2)
long_re_emergence_time_series_2 =
  as.numeric(long_re_emergence_time_series_2 -  as.Date("1986-01-01"))
years_in_long_re_emergence_time_series_2 =
  years_in_long_re_emergence_time_series_2 - 1986

year_table = data.frame(time =
                          long_re_emergence_time_series_2,
                        year =
                          years_in_long_re_emergence_time_series_2)

long_covar_start = min(long_re_emergence_time_series_2) -
  3 * 30

#End covariates 1 week after end of data
log_covar_end = max(long_re_emergence_time_series_2) +
  3 * 30

# Set covariate time step (Default is 1 hour,
# want it smaller than dt to be safe)
long_covar_dt = 1 / 24
t0 = as.numeric(as.Date("1986/05/01") -
                  as.Date("1986/01/01"))

long_covar_times = seq(from = long_covar_start,
                       to = log_covar_end,
                       by = long_covar_dt)

long_covar = covariate_table(
  t = long_covar_times,
  s = periodic.bspline.basis(
    t,
    nbasis = 3,
    degree = 3,
    period = 365,
    name = '%d'
  ),
  times = "t"
)

time_seq = long_covar@times
covar_table = as.data.frame(t(long_covar@table))
head(covar_table)
covar_table_with_time = mutate(covar_table,
                               time = long_covar_times)
covar_at_obs_times = filter(
  covar_table_with_time,
  time %in% long_re_emergence_time_series_2)


table(years_in_long_re_emergence_time_series_2)


re_emergence_threshold = 1


a = Rio_city_DENV1_clean
a$Year = year(a$Date)
total_dengue_cases_by_year = aggregate(
  Y ~ Year, a, FUN = sum)
Re_emergnce_95_observed_cases = filter(
  total_dengue_cases_by_year,
  Year == 1995)$Y
order_of_magnitude_epi_threshold = exp(
  trunc(log(Re_emergnce_95_observed_cases)))

ptm = proc.time()


spark_size_list = c(20)
spark_year_list = c(1990)

relevant_column_data = dplyr::select(
  all_combos, -one_of("seed", "LL",
                      "Profile_Type","R_naught"))

relevant_colnames = colnames(relevant_column_data)

re_emergence_prob_data_all_combos =
  data.frame(matrix(nrow = 0, ncol = 27))

colnames(re_emergence_prob_data_all_combos) = c(
  "spark_size",
  "total_re_emergence_prob_1_year",
  "total_re_emergence_prob_2_year",
  "total_re_emergence_prob_3_year",
  "spark_year",
  "R_naught",
  relevant_colnames,
  "t_stop_immigration",
  "spark",
  "spark_time_start",
  "spark_time_end"
)
for (combo_index in seq(
  1:nrow(param_data_subset))) {
  print("combo_index")
  print(combo_index)
  combo_params = param_data_subset[combo_index,]
  combo_LL = combo_params$LL
  R_naught = combo_params$R_naught
  combo_params = dplyr::select(combo_params,
                               -one_of("seed", "LL",
                                       "Profile_Type",
                                       "R_naught"))
  combo_params$r = human_pop_growth_rate
  combo_params$r = human_pop_growth_rate
  combo_params$t_stop_immigration =
    max(Rio_data_clean$times)
  
  re_emergence_prob_data_all_years_single_combo =
    data.frame(matrix(nrow = 0, ncol = 5))
  colnames(re_emergence_prob_data_all_years_single_combo) =
    c(
    "spark_size ",
    "total_re_emergence_prob_1_year",
    "total_re_emergence_prob_2_year",
    "total_re_emergence_prob_3_year",
    "spark_year"
  )
  total_lik = sum(exp(combo_LL))
  

  for (spark_year_index in seq(
    1:length(spark_year_list))) {
    print(spark_year_index)
    spark_year =
      spark_year_list[spark_year_index]
    
    total_re_emergence_prob_across_all_comb_and_sim_1_year =
      as.numeric(vector(length = length(spark_size_list)))
    total_re_emergence_prob_across_all_comb_and_sim_2_year =
      as.numeric(vector(length = length(spark_size_list)))
    total_re_emergence_prob_across_all_comb_and_sim_3_year =
      as.numeric(vector(length = length(spark_size_list)))
    for (spark_size_index in seq(1:length(spark_size_list))) {
      spark_size = spark_size_list[spark_size_index]
      print("spark_size_index = ")
      print(spark_size_index)
      
      
      combo_params$spark = spark_size
      
      spark_time = as.numeric(
        as.Date(paste0(spark_year, "-01-01")) -
          as.Date("1986-01-01"))
      spark_time_end = as.numeric(
        as.Date(paste0(spark_year, "-02-01")) -
          as.Date("1986-01-01"))
      
      combo_params$spark_time_start =
        spark_time
      combo_params$spark_time_end =
        spark_time_end
      
      sim_data_sample_param =
        simulate(
          nsim = 100,
          seed = 12345,
          times = long_re_emergence_time_series_2,
          t0 = t0,
          rprocess = euler(
            rproc_re_emerge_spark_month_stop_immigration,
            delta.t = 1
        ),
        params = combo_params,
        paramnames =
          paramnames_spark_month_stop_immigration,
        statenames = 
          statenames_spark_month,
        obsnames = obsnames,
        accumvars = acumvarnames,
        covar = long_covar,
        rinit = init_spark_month,
        rmeas = rmeas,
        partrans = par_trans,
        format = "data.frame",
        cdir = '/scratch/rsubramanian/tempdir'
      )
      #head(sim_data)
      
      Beta_t =
        combo_params$Beta_0*(
          1 +
            combo_params$delta*sin(
              combo_params$omega*covar_at_obs_times$time +
                combo_params$phi));
      
      R_0 = (
        Beta_t / (
          combo_params$gamma + combo_params$mu_H
          )) * (combo_params$mu_EI /(
            combo_params$mu_EI + combo_params$mu_H))
      
      sim_data_sample_param =
        join(sim_data_sample_param,
             year_table, by = "time")
      
      year_of_nearest_obs_greater_than_spark_time =
        min(year_table[year_table$time >
                         spark_time, ]$year)
      
      second_year_of_re_emgergence_epi =
        year_of_nearest_obs_greater_than_spark_time + 1
      
      third_year_of_re_emgergence_epi =
        year_of_nearest_obs_greater_than_spark_time + 2
      
      all_sim_probs_unweighted_1_year =
        as.numeric(vector(length = 1))
      all_sim_probs_unweighted_2_year =
        as.numeric(vector(length = 1))
      all_sim_probs_unweighted_3_year =
        as.numeric(vector(length = 1))
      
      for (s in seq(
        1:length(
          unique(
            sim_data_sample_param$.id)))) {
        single_sim_data =
          filter(sim_data_sample_param, .id == s)
        
        sim_data_year_of_re_emergence =
          filter(single_sim_data,
                 year ==
                   year_of_nearest_obs_greater_than_spark_time)
        sim_data_second_year_of_re_emergence =
          filter(single_sim_data,
                 year == 
                   second_year_of_re_emgergence_epi)
        sim_data_third_year_of_re_emergence =
          filter(single_sim_data,
                 year == third_year_of_re_emgergence_epi)
        
        epi_start_time_1_year =
          min(sim_data_year_of_re_emergence$time)
        epi_start_time_2_year =
          min(sim_data_second_year_of_re_emergence$time)
        epi_start_time_3_year =
          min(sim_data_third_year_of_re_emergence$time)
        
        sim_data_at_epi_start_1_year =
          filter(sim_data_year_of_re_emergence,
                 time == epi_start_time_1_year)
        sim_data_at_epi_start_2_year = filter(
          sim_data_second_year_of_re_emergence,
          time == epi_start_time_2_year)
        sim_data_at_epi_start_3_year = filter(
          sim_data_third_year_of_re_emergence,
          time == epi_start_time_3_year)
        
        epi_end_time_1_year = max(
          sim_data_year_of_re_emergence$time)
        epi_end_time_2_years = max(
          sim_data_second_year_of_re_emergence$time)
        epi_end_time_3_years = max(
          sim_data_third_year_of_re_emergence$time)
        
        sim_data_at_epi_end_1_year =
          filter(
            sim_data_year_of_re_emergence,
            time == epi_end_time_1_year)
        sim_data_at_epi_end_2_years =
          filter(sim_data_second_year_of_re_emergence,
                 time == epi_end_time_2_years)
        sim_data_at_epi_end_3_years =
          filter(sim_data_third_year_of_re_emergence,
                 time == epi_end_time_3_years)
        
        dS_over_epidemic_1_year =
          sim_data_at_epi_end_1_year$S -
          sim_data_at_epi_start_1_year$S
        dS_over_epidemic_2_year =
          sim_data_at_epi_end_2_years$S -
          sim_data_at_epi_start_2_year$S
        dS_over_epidemic_3_year =
          sim_data_at_epi_end_3_years$S -
          sim_data_at_epi_start_3_year$S
        
        
        single_sim_spark_time_spark_size_status_1_year =
          as.numeric(dS_over_epidemic_1_year < 0)
        single_sim_spark_time_spark_size_status_2_year =
          as.numeric(dS_over_epidemic_2_year < 0)
        single_sim_spark_time_spark_size_status_3_year =
          as.numeric(dS_over_epidemic_3_year < 0)
        
        all_sim_probs_unweighted_1_year =
          all_sim_probs_unweighted_1_year +
          single_sim_spark_time_spark_size_status_1_year
        all_sim_probs_unweighted_2_year =
          all_sim_probs_unweighted_2_year +
          single_sim_spark_time_spark_size_status_2_year
        all_sim_probs_unweighted_3_year =
          all_sim_probs_unweighted_3_year +
          single_sim_spark_time_spark_size_status_3_year
      }
      all_sim_probs_unweighted_1_year =
        all_sim_probs_unweighted_1_year /
        length(unique(sim_data_sample_param$.id))
      all_sim_probs_unweighted_2_year =
        all_sim_probs_unweighted_2_year /
        length(unique(sim_data_sample_param$.id))
      all_sim_probs_unweighted_3_year =
        all_sim_probs_unweighted_3_year /
        length(unique(sim_data_sample_param$.id))
      
      #Multiply by weight (function of L of parameters)
      all_sim_probs_weighted_1_year =
        all_sim_probs_unweighted_1_year 
      all_sim_probs_weighted_2_year =
        all_sim_probs_unweighted_2_year 
      all_sim_probs_weighted_3_year =
        all_sim_probs_unweighted_3_year 
      
      
      ## Add to total probability 
      ## accross all combinations and simulations
      total_re_emergence_prob_across_all_comb_and_sim_1_year[spark_size_index] =
        total_re_emergence_prob_across_all_comb_and_sim_1_year[spark_size_index] +
        all_sim_probs_weighted_1_year
      total_re_emergence_prob_across_all_comb_and_sim_2_year[spark_size_index] =
        total_re_emergence_prob_across_all_comb_and_sim_2_year[spark_size_index] +
        all_sim_probs_weighted_2_year
      total_re_emergence_prob_across_all_comb_and_sim_3_year[spark_size_index] =
        total_re_emergence_prob_across_all_comb_and_sim_3_year[spark_size_index] +
        all_sim_probs_weighted_3_year
      
      
    }
    re_emergence_prob_data = data.frame(
      spark_size = spark_size_list,
      total_re_emergence_prob_1_year =
        total_re_emergence_prob_across_all_comb_and_sim_1_year,
      total_re_emergence_prob_2_year =
        total_re_emergence_prob_across_all_comb_and_sim_2_year,
      total_re_emergence_prob_3_year =
        total_re_emergence_prob_across_all_comb_and_sim_3_year,
      spark_year = rep(
        spark_year,
        length = length(
          total_re_emergence_prob_across_all_comb_and_sim_1_year)
      )
    )
    re_emergence_prob_data_all_years_single_combo =
      rbind(
        re_emergence_prob_data_all_years_single_combo,
        re_emergence_prob_data)
    
  }
  re_emergence_prob_data_all_years_single_combo$R_naught =
    R_naught
  combo_params$R_naught =
    R_naught
  re_emergence_prob_data_all_years_single_combo = 
    join(
      re_emergence_prob_data_all_years_single_combo,
      combo_params)
  re_emergence_prob_data_all_combos =
    rbind(
      re_emergence_prob_data_all_combos,
      re_emergence_prob_data_all_years_single_combo
  )
}
proc.time() - ptm




write.csv(
  re_emergence_prob_data_all_combos,
  paste0(
    "../Generated_Data/Profiles/",
    model_name,
    "_Model/stoch_re_emerge_test/",
    model_name,
    "_re_mergence_spark_probability_data_subset_",
    param_index,
    ".csv"
  ),
  row.names = FALSE
)
