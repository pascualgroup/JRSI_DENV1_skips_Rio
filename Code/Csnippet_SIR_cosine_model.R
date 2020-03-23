# ---- covar_time_names ----
#Start covariates 1 month before start of data
covar_start = 0

#End covariates 1 month after end of data
covar_end = max(Rio_data_clean$times) + 31

#Set covariate time step (Default is 1 hour, want it smaller than dt to be safe)
covar_dt = 1/24

covar_times = seq(from = covar_start,
                  to = covar_end,
                  by = covar_dt)

# ---- statenames ----
statenames = c("S", "I", "R" , "C", "N")
acumvarnames = c("C")
obsnames = c("Y")

# ---- paramnames ----
paramnames = c("mu_H", "gamma", "N_0", "rho", "Beta_0", "delta","omega","phi", "sigma_P", 
               "sigma_M", "I_0", "R_0", "C_0", "r", "epsilon")

# ---- rproc ----
#Process model Csnippet
rproc <- Csnippet("
                  
                  
                  if(R < 0 || I < 0 ||  N < 0){
                    Rprintf(\"Negative state variable detected at  t = %lg \\n\", t);
                    Rprintf(\"I = %lg \\n\", I);
                    Rprintf(\"R = %lg \\n\", R);
                    Rprintf(\"N = %lg \\n\", N);
                    Rprintf(\"S = %lg \\n\", S);

                  }

                  if(isnan(R) || isnan(I) || isnan(N) || isnan(S)){
                    Rprintf(\"nan state variable detected at top of process model t = %lg \\n\", t);
                    Rprintf(\"I = %lg \\n\", I);
                    Rprintf(\"R = %lg \\n\", R);
                    Rprintf(\"N = %lg \\n\", N);
                    Rprintf(\"S = %lg \\n\", S);

                  }


                  double beta = Beta_0*(1 + delta*sin(omega*t + phi));
                  double dW = rgammawn(sigma_P,dt);
                  double lambda = beta*((I+ epsilon)/N)*dW;
                  //Rprintf(\"start proc t = %lg \\n\", t);
                  //Rprintf(\"beta = %lg \\n\", beta);
                  //Rprintf(\"dW = %lg \\n\", dW);
                  
                  //Rprintf(\"lambda = %lg \\n\", lambda);
                  
                  //Rprintf(\"S = %lg \\n\", S);
                  // Rprintf(\"I = %lg \\n\", I);
                  //Rprintf(\"C = %lg \\n\", C);
                  // Rprintf(\"rho = %lg \\n\", rho);
                  
                  double dSI = rbinom(S, 1 - exp(-lambda));

                  double dIR = rbinom(I, 1 - exp(-gamma*dt));
                  double dBS = rbinom(N, 1 - exp(-mu_H*dt));
                  
                  //Add population growth
                  double dBS_N = rbinom(N, 1 - exp(-r*dt));
                  //if(t < 10){
                  //Rprintf(\"r = %lg \\n\", r);
                  //Rprintf(\"N = %lg \\n\", N);
                  //Rprintf(\"t = %lg \\n\", t);
                  //Rprintf(\"dBS_N = %lg \\n\", dBS_N);
                  //}
                  
                  
                  //Transition increments
                  S += dBS  + dBS_N - dSI;
                  I += dSI - dIR;
                  R += dIR;
                  N += dBS + dBS_N;
                  
                  double dSM = rbinom(S, 1 - exp(-mu_H*dt));
                  double dIM = rbinom(I, 1 - exp(-mu_H*dt));
                  double dRM = rbinom(R, 1 - exp(-mu_H*dt));
                  S += - dSM;
                  I +=  - dIM;
                  R += - dRM;
                  N +=  - dSM - dIM - dRM;
                  
                  
                  //Rprintf(\"dSI = %lg \\n\", dSI);
                  
                  //Rprintf(\"dIR = %lg \\n\", dIR);
                  
                  
                  
                  C += rho*dSI;
                  if(C < 0 || S < 0 || I < 0 || R < 0 ){
                  Rprintf(\"Neg value at t = %lg \\n\", t);
                  //    Rprintf(\"beta = %lg \\n\", beta);
                  //    Rprintf(\"dSI = %lg \\n\", dSI);
                  Rprintf(\"S = %lg \\n\", S);
                  Rprintf(\"I = %lg \\n\", I);
                  
                  Rprintf(\"I = %lg \\n\", I);
                  //    Rprintf(\"dSI = %lg \\n\", dSI);
                  //    Rprintf(\"dIR = %lg \\n\", dIR);
                  //    Rprintf(\"C = %lg \\n\", C);
                  //    Rprintf(\"rho = %lg \\n\", rho);
                  
                  I = 0;
                  
                  }
                  
                  if(isnan(R) || isnan(I) ||isnan(N) || isnan(S)){
                    Rprintf(\"nan state variable detected at bottom of process model t = %lg \\n\", t);
                    Rprintf(\"I = %lg \\n\", I);
                    Rprintf(\"R = %lg \\n\", R);
                    Rprintf(\"N = %lg \\n\", N);
                    Rprintf(\"S = %lg \\n\", S);
                    Rprintf(\"lambda = %lg \\n\", lambda);
                    Rprintf(\"Beta_0 = %lg \\n\", Beta_0);
                    Rprintf(\"delta = %lg \\n\", delta);
                    Rprintf(\"phi = %lg \\n\", phi);
                    Rprintf(\"rho = %lg \\n\", rho);
                    Rprintf(\"I_0 = %lg \\n\", I_0);
                    Rprintf(\"R_0 = %lg \\n\", R_0);


                                    }

                  ")


# ---- rmeas ----
rmeas <- Csnippet("
                  double size = 1.0/sigma_M/sigma_M;
                  Y = rnbinom_mu(size,C);
                  ")


# ---- dmeas ----
dmeas <- Csnippet("
                  double size = 1.0/sigma_M/sigma_M;
                  static double tol = 0.1;
                  lik = dnbinom_mu(Y,size,C+tol,1);
                  
                  double total_0 = round(I_0) + round(R_0);
                  if(total_0 > round(N_0)){
                    lik = -40;
                  
                  }


                if(R_0 < 0 || I_0 < 0 || N_0 < 0){
                    lik = -40;
                  
                  }


                  //Debugging Print Code
                  //Rprintf(\"t = %lg \\n\", t);
                  //Rprintf(\"I = %lg \\n\", I);

                  //Rprintf(\"Lik = %lg \\n\", lik);
                  //Rprintf(\"Y = %lg \\n\", Y);
                  //Rprintf(\"C = %lg \\n\", C);
                  //Rprintf(\"tol = %lg \\n\", tol);
                  //Rprintf(\"size = %lg \\n\", size);
                  
                  
                  if (!give_log) lik = exp(lik);
                  ")


# ---- init ----
init <- Csnippet("
                 //Rprintf(\"At init N_0 = %lg \\n\", N_0);
                 //Rprintf(\"At init rho = %lg \\n\", rho);
                 double total_0 = round(I_0) + round(R_0);
                 if(total_0 > round(N_0)){
                  S = 0;
                  I = 0;
                  R = round(N_0);
                  
                 }
                 if(I_0 > N_0){
                 I = round(N_0);
                 S = 0;
                 N = round(N_0);
                 R = 0;
                 }else{
                 
                 if(R_0 > N_0){
                 I = 0;
                 S = 0;
                 R = round(N_0);
                 }else{
                    if(R_0 < 0 || I_0 < 0 || N_0 < 0){
                      I = 0;
                      N = 1;
                      R = 0;
                      S = 1;
                    } else{
                        I = round(I_0);
                        N = round(N_0);
                        R = round(R_0);
                        S = round(N_0)-round(I_0) - round(R_0);
                    }
                 }
                 
                 }
                 
                 
                 
                 C = C_0;
                 //Rprintf(\"At init N = %lg \\n\", N);
                 //Rprintf(\"At init I = %lg \\n\", I);
                 //Rprintf(\"At init C = %lg \\n\", C);

                 ")

# ---- par_trans ----
par_trans = parameter_trans(log = c("mu_H", "Beta_0", "gamma", "sigma_P", "sigma_M", "r", "epsilon"),
                            logit = c("rho", "delta"))

# ---- covar ----
covar=covariate_table(
  t=covar_times,
  s=periodic.bspline.basis(t,nbasis=3,degree=3,period=365, name='%d'),
  times="t"
)

# ---- MIF_and_pfilter_output_function ----
get_MIF_final_params_and_pfilter_LL = function(data,
                                               times,
                                               t0, 
                                               rproc,
                                               params,
                                               paramnames,
                                               statenames,
                                               obsnames,
                                               dmeas,
                                               accumvars,
                                               init,
                                               rmeas,
                                               par_trans,
                                               Np,
                                               Nmif ,
                                               cooling.fraction.50,
                                               rw.sd ,
                                               delta_time,
                                               param_index,
                                               i,
                                               detail_log = FALSE,
                                               covar) {
  
  log_str = ""
  if(detail_log == TRUE){
    log_str = paste0(log_str, 
                     "subset:", param_index,
                     " comb: ", i,
                     " starting_param_guess: ", names(params)," = " ,params,"\n")
  }
  
  
  seed <- round(runif(1,min=1,max=2^30))
  #Compute MIF calculation
  mf <- tryCatch(
    mif2(
      data = data,
      times = times,
      t0 = t0,
      seed = seed,
      rprocess = pomp2::euler(rproc, delta.t = delta_time),
      params = params,
      paramnames = paramnames,
      statenames = statenames,
      obsnames = obsnames,
      dmeas = dmeas,
      accumvars = accumvars,
      covar=covar,
      rinit = init,
      rmeas = rmeas,
      partrans = par_trans,
      start = params,
      Np = Np,
      Nmif = Nmif,
      cooling.fraction.50 = cooling.fraction.50,
      rw.sd = rw.sd
    ),
    error = function(e) e
  )
  MIF_single_param_output = params
  MIF_single_param_output$LL = NA
  if(detail_log == TRUE){
    log_str = paste0(log_str, "mif warnings: \n ", warnings(), " \n Done with warnings \n")
  }
  
  
  if(!inherits(mf, "error")){
    if(length(coef(mf)) > 0){
      print(mf)
      if(detail_log == TRUE){
        log_str = paste0(log_str, "subset:", param_index,
                         " comb: ", i,
                         " mif_end_guess: ", names(params)," = " ,coef(mf),"\n")
        
        log_str = paste0(log_str, "subset:", param_index,
                         " comb: ", i,
                         " mif_nfail: ", mf@nfail," mif_ess: " ,
                         eff.sample.size(mf),
                         " MIF Log Lik: ", logLik(mf),"\n")
      }
      MIF_single_param_output = as.data.frame(t(coef(mf)))
      ll <- tryCatch(
        replicate(n=10,logLik(pfilter(
          data = data,
          times = times,
          t0 = t0,
          rprocess = pomp2::euler(rproc,delta.t = delta_time),
          paramnames = paramnames,
          statenames = statenames,
          obsnames = obsnames,
          dmeas = dmeas,
          accumvars = accumvars,
          covar = covar,
          rinit = init,
          rmeas = rmeas,
          partrans = par_trans,
          format = "data.frame",
          Np=Np,
          params=coef(mf)))),
        error = function(e) e
      )
      if(is(ll,"error")) {}else{
        ll <- logmeanexp(ll)
        if(detail_log == TRUE){
          log_str = paste0(log_str, "pfilter_warnings: \n ", warnings(), " \n Done with warnings \n")
        }
        
      }
      if(is.na(ll)) {}else{
        MIF_single_param_output$LL = ll
      }
    }
  }
  #return_list = list(MIF_single_param_output, mf)
  #return(return_list)
  if(detail_log == TRUE){
    log_str = paste0(log_str, "subset:", param_index,
                     " comb: ", i,
                     " end_param_guess: ", colnames(MIF_single_param_output)," = " ,
                     MIF_single_param_output,"\n")
  }
  log_str_collapsed = paste0(log_str, collapse = " ")
  
  MIF_single_param_output$seed = seed
  MIF_single_param_output$Log = log_str_collapsed
  return(MIF_single_param_output)
}

# ---- rproc_re_emerge_spark ----
#Process model Csnippet
rproc_re_emerge_spark <- Csnippet("
                  
                  if(t > spark_time){
                    if(spark_activated == FALSE){
                      //Rprintf(\"Activating spark at time t = %lg \\n\", t);
                      //Rprintf(\"Spark time is  = %lg \\n\", spark_time);
                      //Rprintf(\"Old value of I is  = %lg \\n\", I);
                      //Rprintf(\"spark_activated is   = %lg \\n\", spark_activated);

                      I += spark;
                      N += spark;
                      //Rprintf(\"New value of I is  = %lg \\n\", I);


                      spark_activated = TRUE;
                      //Rprintf(\"spark_activated is   = %lg \\n\", spark_activated);

                    }
                  }
                  double beta = Beta_0*(1 + delta*sin(omega*t + phi));
                  double dW = rgammawn(sigma_P,dt);
                  double lambda = beta*((I+ epsilon)/N)*dW;
                  //Rprintf(\"start proc t = %lg \\n\", t);
                  //Rprintf(\"beta = %lg \\n\", beta);
                  //Rprintf(\"dW = %lg \\n\", dW);
                  
                  //Rprintf(\"lambda = %lg \\n\", lambda);
                  
                  //Rprintf(\"S = %lg \\n\", S);
                  // Rprintf(\"I = %lg \\n\", I);
                  //Rprintf(\"C = %lg \\n\", C);
                  // Rprintf(\"rho = %lg \\n\", rho);
                  
                  double dSI = rbinom(S, 1 - exp(-lambda));

                  double dIR = rbinom(I, 1 - exp(-gamma*dt));
                  double dBS = rbinom(N, 1 - exp(-mu_H*dt));
                  
                  //Add population growth
                  double dBS_N = rbinom(N, 1 - exp(-r*dt));
                  //if(t < 10){
                  //Rprintf(\"r = %lg \\n\", r);
                  //Rprintf(\"N = %lg \\n\", N);
                  //Rprintf(\"t = %lg \\n\", t);
                  //Rprintf(\"dBS_N = %lg \\n\", dBS_N);
                  //}
                  
                  
                  //Transition increments
                  S += dBS  + dBS_N - dSI;
                  I += dSI - dIR;
                  R += dIR;
                  N += dBS + dBS_N;
                  
                  double dSM = rbinom(S, 1 - exp(-mu_H*dt));
                  double dIM = rbinom(I, 1 - exp(-mu_H*dt));
                  double dRM = rbinom(R, 1 - exp(-mu_H*dt));
                  S += - dSM;
                  I +=  - dIM;
                  R += - dRM;
                  N +=  - dSM - dIM - dRM;
                  
                  
                  //Rprintf(\"dSI = %lg \\n\", dSI);
                  
                  //Rprintf(\"dIR = %lg \\n\", dIR);
                  
                  
                  
                  C += rho*dSI;
                  if(C < 0 || S < 0 || I < 0 || R < 0 ){
                  Rprintf(\"Neg value at t = %lg \\n\", t);
                  //    Rprintf(\"beta = %lg \\n\", beta);
                  //    Rprintf(\"dSI = %lg \\n\", dSI);
                  Rprintf(\"S = %lg \\n\", S);
                  Rprintf(\"I = %lg \\n\", I);
                  
                  Rprintf(\"I = %lg \\n\", I);
                  //    Rprintf(\"dSI = %lg \\n\", dSI);
                  //    Rprintf(\"dIR = %lg \\n\", dIR);
                  //    Rprintf(\"C = %lg \\n\", C);
                  //    Rprintf(\"rho = %lg \\n\", rho);
                  
                  I = 0;
                  
                  }
                  
                  
                  ")


# ---- rproc_re_emerge_spark_stop_immigration ----
#Process model Csnippet
rproc_re_emerge_spark_stop_immigration <- Csnippet("
                                  
                                  if(t > spark_time){
                                  if(spark_activated == FALSE){
                                  //Rprintf(\"Activating spark at time t = %lg \\n\", t);
                                  //Rprintf(\"Spark time is  = %lg \\n\", spark_time);
                                  //Rprintf(\"Old value of I is  = %lg \\n\", I);
                                  //Rprintf(\"spark_activated is   = %lg \\n\", spark_activated);
                                  
                                  I += spark;
                                  N += spark;
                                  //Rprintf(\"New value of I is  = %lg \\n\", I);
                                  
                                  
                                  spark_activated = TRUE;
                                  //Rprintf(\"spark_activated is   = %lg \\n\", spark_activated);
                                  
                                  }
                                  }
                                  double beta = Beta_0*(1 + delta*sin(omega*t + phi));
                                  double dW = rgammawn(sigma_P,dt);
                                  if(t < t_stop_immigration){
                                    double lambda = beta*((I+ epsilon)/N)*dW;

                                  }else{
                                    double lambda = beta*((I)/N)*dW;
                                  }
                                  //Rprintf(\"start proc t = %lg \\n\", t);
                                  //Rprintf(\"beta = %lg \\n\", beta);
                                  //Rprintf(\"dW = %lg \\n\", dW);
                                  
                                  //Rprintf(\"lambda = %lg \\n\", lambda);
                                  
                                  //Rprintf(\"S = %lg \\n\", S);
                                  // Rprintf(\"I = %lg \\n\", I);
                                  //Rprintf(\"C = %lg \\n\", C);
                                  // Rprintf(\"rho = %lg \\n\", rho);
                                  
                                  double dSI = rbinom(S, 1 - exp(-lambda));

                                  double dIR = rbinom(I, 1 - exp(-gamma*dt));
                                  double dBS = rbinom(N, 1 - exp(-mu_H*dt));
                                  
                                  //Add population growth
                                  double dBS_N = rbinom(N, 1 - exp(-r*dt));
                                  //if(t < 10){
                                  //Rprintf(\"r = %lg \\n\", r);
                                  //Rprintf(\"N = %lg \\n\", N);
                                  //Rprintf(\"t = %lg \\n\", t);
                                  //Rprintf(\"dBS_N = %lg \\n\", dBS_N);
                                  //}
                                  
                                  
                                  //Transition increments
                                  S += dBS  + dBS_N - dSI;
                                  I += dSI - dIR;
                                  R += dIR;
                                  N += dBS + dBS_N;
                                  
                                  double dSM = rbinom(S, 1 - exp(-mu_H*dt));
                                  double dIM = rbinom(I, 1 - exp(-mu_H*dt));
                                  double dRM = rbinom(R, 1 - exp(-mu_H*dt));
                                  S += - dSM;
                                  I +=  - dIM;
                                  R += - dRM;
                                  N +=  - dSM - dIM - dRM;
                                  
                                  
                                  //Rprintf(\"dSI = %lg \\n\", dSI);
                                  
                                  //Rprintf(\"dIR = %lg \\n\", dIR);
                                  
                                  
                                  
                                  C += rho*dSI;
                                  if(C < 0 || S < 0 || I < 0 || R < 0 ){
                                  Rprintf(\"Neg value at t = %lg \\n\", t);
                  //    Rprintf(\"beta = %lg \\n\", beta);
                  //    Rprintf(\"dSI = %lg \\n\", dSI);
                  Rprintf(\"S = %lg \\n\", S);
                  Rprintf(\"I = %lg \\n\", I);
                  
                  Rprintf(\"I = %lg \\n\", I);
                  //    Rprintf(\"dSI = %lg \\n\", dSI);
                  //    Rprintf(\"dIR = %lg \\n\", dIR);
                  //    Rprintf(\"C = %lg \\n\", C);
                  //    Rprintf(\"rho = %lg \\n\", rho);
                  
                  I = 0;
                  
                  }
                  
                  
                  ")


# ---- init_spark ----
init_spark <- Csnippet("
                 //Rprintf(\"At init N_0 = %lg \\n\", N_0);
                 //Rprintf(\"At init rho = %lg \\n\", rho);
                spark_activated = FALSE;
                 double total_0 = round(I_0) + round(R_0);
                 if(total_0 > round(N_0)){
                 S = 0;
                 I = 0;
                 R = round(N_0);
                 
                 }
                 if(I_0 > N_0){
                 I = round(N_0);
                 S = 0;
                 N = round(N_0);
                 R = 0;
                 }else{
                 if(R_0 > N_0){
                 I = 0;
                 S = 0;
                 R = round(N_0);
                 }else{
                    if(R_0 < 0 || I_0 < 0 || N_0 < 0){
                        I = 0;
                        N = 1;
                        R = 0;
                        S = 1;
                      } else{
                          I = round(I_0);
                          N = round(N_0);
                          R = round(R_0);
                          S = round(N_0)-round(I_0) - round(R_0);
                      }
                 }
                 
                 }
                 
                 
                 
                 C = C_0;
                 //Rprintf(\"At init N = %lg \\n\", N);
                 //Rprintf(\"At init I = %lg \\n\", I);
                 //Rprintf(\"At init C = %lg \\n\", C);

                 ")



# ---- statenames_spark ----
statenames_spark = c("S", "I", "R" , "C", "N", "spark_activated")


# ---- paramnames_spark ----
paramnames_spark = c("mu_H", "gamma", "N_0", "rho", "Beta_0", "delta","omega","phi", "sigma_P", 
               "sigma_M", "I_0", "R_0", "C_0", "r", "epsilon", "spark", "spark_time")




# ---- rproc_re_emerge_spark_month ----
#Process model Csnippet
rproc_re_emerge_spark_month <- Csnippet("
                                  
                                  if(t > spark_time_start){
                                  if(t < spark_time_end ){
                                  //Rprintf(\"Activating spark at time t = %lg \\n\", t);
                                  //Rprintf(\"Spark time is  = %lg \\n\", spark_time);
                                  //Rprintf(\"Old value of I is  = %lg \\n\", I);
                                  //Rprintf(\"spark_activated is   = %lg \\n\", spark_activated);
                                  
                                  I += spark;
                                  N += spark;
                                  //Rprintf(\"New value of I is  = %lg \\n\", I);
                                  
                                  
                                  //Rprintf(\"spark_activated is   = %lg \\n\", spark_activated);
                                  
                                  }
                                  }
                                  double beta = Beta_0*(1 + delta*sin(omega*t + phi));
                                  double dW = rgammawn(sigma_P,dt);
                                  double lambda = beta*((I+ epsilon)/N)*dW;
                                  //Rprintf(\"start proc t = %lg \\n\", t);
                                  //Rprintf(\"beta = %lg \\n\", beta);
                                  //Rprintf(\"dW = %lg \\n\", dW);
                                  
                                  //Rprintf(\"lambda = %lg \\n\", lambda);
                                  
                                  //Rprintf(\"S = %lg \\n\", S);
                                  // Rprintf(\"I = %lg \\n\", I);
                                  //Rprintf(\"C = %lg \\n\", C);
                                  // Rprintf(\"rho = %lg \\n\", rho);
                                  
                                  double dSI = rbinom(S, 1 - exp(-lambda));

                                  double dIR = rbinom(I, 1 - exp(-gamma*dt));
                                  double dBS = rbinom(N, 1 - exp(-mu_H*dt));
                                  
                                  //Add population growth
                                  double dBS_N = rbinom(N, 1 - exp(-r*dt));
                                  //if(t < 10){
                                  //Rprintf(\"r = %lg \\n\", r);
                                  //Rprintf(\"N = %lg \\n\", N);
                                  //Rprintf(\"t = %lg \\n\", t);
                                  //Rprintf(\"dBS_N = %lg \\n\", dBS_N);
                                  //}
                                  
                                  
                                  //Transition increments
                                  S += dBS  + dBS_N - dSI;
                                  I += dSI - dIR;
                                  R += dIR;
                                  N += dBS + dBS_N;
                                  
                                  double dSM = rbinom(S, 1 - exp(-mu_H*dt));
                                  double dIM = rbinom(I, 1 - exp(-mu_H*dt));
                                  double dRM = rbinom(R, 1 - exp(-mu_H*dt));
                                  S += - dSM;
                                  I +=  - dIM;
                                  R += - dRM;
                                  N +=  - dSM - dIM - dRM;
                                  
                                  
                                  //Rprintf(\"dSI = %lg \\n\", dSI);
                  
                  //Rprintf(\"dIR = %lg \\n\", dIR);
                  
                  
                  
                  C += rho*dSI;
                  if(C < 0 || S < 0 ||E < 0 || I < 0 || R < 0 ){
                  Rprintf(\"Neg value at t = %lg \\n\", t);
                  //    Rprintf(\"beta = %lg \\n\", beta);
                  //    Rprintf(\"dSI = %lg \\n\", dSI);
                  Rprintf(\"S = %lg \\n\", S);

                  Rprintf(\"I = %lg \\n\", I);
                  //    Rprintf(\"dSI = %lg \\n\", dSI);
                  //    Rprintf(\"dIR = %lg \\n\", dIR);
                  //    Rprintf(\"C = %lg \\n\", C);
                  //    Rprintf(\"rho = %lg \\n\", rho);
                  
                  I = 0;
                  
                  }
                  
                  
                  ")

# ---- rproc_re_emerge_spark_month_stop_immigration ----
rproc_re_emerge_spark_month_stop_immigration <- Csnippet("
                                  
                                        if(t > spark_time_start){
                                        if(t < spark_time_end ){
                                        //Rprintf(\"Activating spark at time t = %lg \\n\", t);
                                        //Rprintf(\"Spark time is  = %lg \\n\", spark_time);
                                        //Rprintf(\"Old value of I is  = %lg \\n\", I);
                                        //Rprintf(\"spark_activated is   = %lg \\n\", spark_activated);
                                        
                                        I += spark;
                                        N += spark;
                                        //Rprintf(\"New value of I is  = %lg \\n\", I);
                                        
                                        
                                        //Rprintf(\"spark_activated is   = %lg \\n\", spark_activated);
                                        
                                        }
                                        }
                                        double beta = Beta_0*(1 + delta*sin(omega*t + phi));
                                        double dW = rgammawn(sigma_P,dt);
                                        double lambda = beta*((I)/N)*dW;
                                        if(t < t_stop_immigration){
                                          lambda = beta*((I+ epsilon)/N)*dW;
                                        }else{}
                                        //Rprintf(\"start proc t = %lg \\n\", t);
                                        //Rprintf(\"beta = %lg \\n\", beta);
                                        //Rprintf(\"dW = %lg \\n\", dW);
                                        
                                        //Rprintf(\"lambda = %lg \\n\", lambda);
                                        
                                        //Rprintf(\"S = %lg \\n\", S);
                                        // Rprintf(\"I = %lg \\n\", I);
                                        //Rprintf(\"C = %lg \\n\", C);
                                        // Rprintf(\"rho = %lg \\n\", rho);
                                        
                                        double dSI = rbinom(S, 1 - exp(-lambda));

                                        double dIR = rbinom(I, 1 - exp(-gamma*dt));
                                        double dBS = rbinom(N, 1 - exp(-mu_H*dt));
                                        
                                        //Add population growth
                                        double dBS_N = rbinom(N, 1 - exp(-r*dt));
                                        //if(t < 10){
                                        //Rprintf(\"r = %lg \\n\", r);
                                        //Rprintf(\"N = %lg \\n\", N);
                                        //Rprintf(\"t = %lg \\n\", t);
                                        //Rprintf(\"dBS_N = %lg \\n\", dBS_N);
                                        //}
                                        
                                        
                                        //Transition increments
                                        S += dBS  + dBS_N - dSI;
                                        I += dSI - dIR;
                                        R += dIR;
                                        N += dBS + dBS_N;
                                        
                                        double dSM = rbinom(S, 1 - exp(-mu_H*dt));
                                        double dIM = rbinom(I, 1 - exp(-mu_H*dt));
                                        double dRM = rbinom(R, 1 - exp(-mu_H*dt));
                                        S += - dSM;
                                        I +=  - dIM;
                                        R += - dRM;
                                        N +=  - dSM - dIM - dRM;
                                        
                                        
                                        //Rprintf(\"dSI = %lg \\n\", dSI);
                                        
                                        //Rprintf(\"dIR = %lg \\n\", dIR);
                                        
                                        
                                        
                                        C += rho*dSI;
                                        if(C < 0 || S < 0 || I < 0 || R < 0 ){
                                        Rprintf(\"Neg value at t = %lg \\n\", t);
                  //    Rprintf(\"beta = %lg \\n\", beta);
                  //    Rprintf(\"dSI = %lg \\n\", dSI);
                  Rprintf(\"S = %lg \\n\", S);

                  Rprintf(\"I = %lg \\n\", I);
                  //    Rprintf(\"dSI = %lg \\n\", dSI);
                  //    Rprintf(\"dIR = %lg \\n\", dIR);
                  //    Rprintf(\"C = %lg \\n\", C);
                  //    Rprintf(\"rho = %lg \\n\", rho);
                  
                  I = 0;
                  
                  }
                  
                  
                  ")

# ---- init_spark_month ----
init_spark_month <- Csnippet("
                       //Rprintf(\"At init N_0 = %lg \\n\", N_0);
                       //Rprintf(\"At init rho = %lg \\n\", rho);
                       double total_0 = round(I_0) + round(R_0);
                       if(total_0 > round(N_0)){
                       S = 0;
                       I = 0;
                       R = round(N_0);
                       
                       }
                       if(I_0 > N_0){
                       I = round(N_0);
                       S = 0;
                       N = round(N_0);
                       R = 0;
                       }else{
                       if(R_0 > N_0){
                       I = 0;
                       S = 0;
                       R = round(N_0);
                       }else{
                          if(R_0 < 0 || I_0 < 0 || N_0 < 0){
                            I = 0;
                            N = 1;
                            R = 0;
                            S = 1;
                          } else{ 
                                I = round(I_0);
                                N = round(N_0);
                                R = round(R_0);
                                S = round(N_0)-round(I_0) - round(R_0);
                              }
                       }
                       
                       }
                       
                       
                       
                       C = C_0;
                       //Rprintf(\"At init N = %lg \\n\", N);
                 //Rprintf(\"At init I = %lg \\n\", I);
                 //Rprintf(\"At init C = %lg \\n\", C);

                 ")




# ---- statenames_spark_month ----
statenames_spark_month = c("S", "I", "R" , "C", "N")

# ---- paramnames_spark ----
paramnames_spark_month = c("mu_H", "gamma", "N_0", "rho", "Beta_0", "delta","omega","phi", "sigma_P", 
                     "sigma_M", "I_0", "R_0", "C_0", "r", "epsilon", "spark", "spark_time_start",
                     "spark_time_end")

# ---- paramnames_spark_month_stop_immigration ----
paramnames_spark_month_stop_immigration = c("mu_H", "gamma", "N_0", "rho", "Beta_0", "delta","omega", "phi", "sigma_P", 
                           "sigma_M", "I_0", "R_0", "C_0", "r", "epsilon", "spark", "spark_time_start",
                           "spark_time_end", "t_stop_immigration")



