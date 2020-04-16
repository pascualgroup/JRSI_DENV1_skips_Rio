rm(list = ls())
source("load_libraries_essential.R")
library(zoo)
library(pomp)
source("rahul_theme.R")
args = commandArgs(trailingOnly = TRUE)

#model_name = as.character(args[1])
model_name = "A_7"
print(model_name)

Rio_data_clean = read.csv("../Generated_Data/Rio_DENV1_Data_3_75_years_clean.csv")
head(Rio_data_clean)
Rio_clean_data = Rio_data_clean
t0 = as.numeric(as.Date("1986/05/01") - as.Date("1986/01/01"))
load(file = "../Down_Data/denguerj1986-2017.RData")
Rio_city_DENV1_clean = data.frame(Y = as.matrix(dengue.ts),
                                  Date = as.Date(as.yearmon(time(dengue.ts))))
Rio_city_DENV1_clean = filter(Rio_city_DENV1_clean, Date >= "1986-05-01")

head(Rio_city_DENV1_clean)
Rio_city_DENV1_clean$Date = Rio_city_DENV1_clean$Date %m+% months(1)
#add_a_week = Rio_city_DENV1_clean$Date %m+% weeks(1)
#last_day_of_month = add_a_month - 1


head(Rio_city_DENV1_clean)


Population_Rio_2000 = 5857904 #Census
Population_Rio_1991 = 5480768# Census:
Two_hour_segments_in_year = 365 * 12
time_between_census_dates = 2000 * 365 - 1991 * 365
human_pop_growth_rate = (1 / time_between_census_dates) *
  log(Population_Rio_2000 / Population_Rio_1991)
human_pop_growth_rate

#Source Csnippets
source(file = "Csnippet_SIR_cosine_model.R")


all_combos = read.csv(
  paste0("../Generated_Data/Profiles/", model_name,
         "_Model/combined_",model_name,
         "_profile_data_directory_with_mean_R_0.csv"))

MLE_params = filter(all_combos, LL == max(LL))

bio_good_2_LL = filter(all_combos, LL > max(all_combos$LL) - 2 )

within_20_LL = filter(all_combos, LL > max(all_combos$LL) - 20 )
test_param_index = 1
single_test_subset_output = read.csv(
  paste0(
    "../Generated_Data/Profiles/",
    model_name,
    "_Model/spark_month_heat_maps/",
    model_name,
    "_re_mergence_spark_probability_data_subset_", test_param_index,
    ".csv"
  ))
all_param_spark_data = data.frame(matrix(nrow = 0, ncol = ncol(single_test_subset_output)))
colnames(all_param_spark_data) = colnames(single_test_subset_output)
head(all_param_spark_data)
num_param_combinations = 457
for(param_index in c(seq(1:23),seq(from = 25,to = 168),seq(from = 170,to = 450),
                     seq(from = 452, to = num_param_combinations))){
  #param_index = as.numeric(Sys.getenv("MOAB_JOBARRAYINDEX"))
  print("param_index")
  print(param_index)
  gardner_max_jobs = 500
  group_size = ceiling(nrow(bio_good_2_LL) / gardner_max_jobs)
  start_index = (param_index - 1) * group_size + 1
  end_index = param_index * group_size
  Num_mif_runs_per_start = 5
  param_data_subset = bio_good_2_LL[start_index:end_index, ]
  
  single_subset_output = read.csv(
    paste0(
      "../Generated_Data/Profiles/",
      model_name,
      "_Model/sup_fig_stoch_re_emergence_test/",
      model_name,
      "_re_mergence_spark_probability_data_subset_", param_index,
      ".csv"
    ))
  if(sum(is.na(single_subset_output$total_re_emergence_prob_1_year)) > 0) {
    print(paste0("Param set fail at ", param_index))
  }
  
  all_param_spark_data = rbind(all_param_spark_data, single_subset_output)
  
}

# ## Save data (FILE IS LARGE SO COMMENTED OUT)
# write.csv(all_param_spark_data, file = paste0("../Generated_Data/Profiles/",
#           model_name,
#           "_Model/sup_fig_stoch_re_emergence_test/",
#           model_name,
#           "_re_mergence_spark_prob_all_params.csv"
# ))

spark_data_90_only = filter(all_param_spark_data, spark_year == 1990)

spark_90_size_20 = filter(spark_data_90_only, spark_size == 20)
no_na = na.omit(spark_data_90_only)
p = ggplot(data = no_na,
           aes(x =R_naught, y =  total_re_emergence_prob_1_year)) + geom_point() +
  rahul_theme+ theme_white_background
p
ML_df = MLE_params
ML_df$r = unique(no_na$r)
ML_with_re_emerge_prob = join(ML_df, no_na)
p = ggplot(data = no_na,
           aes(x =sigma_P, y = total_re_emergence_prob_1_year,
               color = spark_size)) + geom_point(size = 3) +
  rahul_theme + theme_white_background + xlab(expression(paste(" Process Noise ", (sigma[P])))) +
  ylab("Re-Emergence \n Probability in 1990") +
  rahul_man_figure_theme +
  geom_point(data = ML_with_re_emerge_prob,
             aes(x = sigma_P, y = total_re_emergence_prob_1_year),
             color = 'red', fill = "NA", size = 5, shape = 21, stroke = 3)
  
p
pdf("../Figures/Supplemental_Figures/Supplemental_Figure_14/Sup_Fig_14.pdf",
    height = 5, width = 10)
print(p)
dev.off()


p = ggplot(data = no_na_20,
           aes(x =sigma_P, y = R_naught, color =  total_re_emergence_prob_1_year)) + geom_point() +
  rahul_theme
p

p = ggplot(data = no_na_20,
           aes(x =rho, y = total_re_emergence_prob_1_year)) + geom_point(size = 3) +
  rahul_theme + theme_white_background + xlab(expression(sigma[P])) + ylab("Re-Emergence \n Probability in 1990") +
  rahul_man_figure_theme +
  geom_point(data = ML_with_re_emerge_prob,
             aes(x = sigma_P, y = total_re_emergence_prob_1_year),
             color = 'red', fill = "NA", size = 5, shape = 21, stroke = 3)

p