data {
  int<lower=1> N;  
  int<lower=1> S;   
  int<lower=0> Y;   
  int<lower=1> R;   
  int<lower=1> n_c;
  int<lower=0> num[N];  
  int<lower=0> den[N];            
  vector[N] time;
  // int<lower = 0> year[N];
  matrix[N,n_c] covariates;
  // country-level data
  int<lower=1> K;
  int<lower=1> K_b;
  int<lower=1> C;
  int<lower=1, upper=C> country_idx[N];
  int<lower=1, upper=S> survey_idx[N];
  int<lower=1, upper=Y> year_idx[N];
  matrix[C,K] country_data;
  matrix[C,K_b] country_data_baseline;

  // useful to match region/country/survey
  int<lower = 1> lookup_cnt[C];
  int<lower = 1> lookup_svy[S];
}
parameters {
  real mu;
  real mu_d;
  vector[n_c] betas;
  // real<lower=0,upper=1> phi_raw;  // used to construct RW(1) coefficient
  real<lower=-1,upper=1> phi;
  vector[Y] a_t;
  vector[S] re_survey;
  vector[C] a_c;
  vector[C] delta_cnt;
  vector[R] a_r;
  vector[R] delta_r;
  
  real<lower=0> sd_t;
  real<lower=0> sd_re_cnt[R];
  real<lower=0> sd_rs_cnt[R];
  real<lower=0> sd_re_survey[R];
  real<lower=0> sd_re_region;
  real<lower=0> sd_rs_region;
  
}
transformed parameters {
  vector[N] fixed_cov = covariates*betas;
}
model {

  mu ~ normal(-1, 2);
  mu_d ~ normal(-.5, 2);
  beta ~ normal(0, 5);
  gamma ~ normal(0, 5);
  

  sd_t ~ normal(0,5);
  phi ~ uniform(-1,1);

  sd_re_cnt ~ cauchy(0,2);
  sd_rs_cnt ~ cauchy(0,2);
  sd_re_region ~ cauchy(0,2);
  sd_rs_region ~ cauchy(0,1);
  sd_re_survey ~ cauchy(0, 1);
  
  // Random intercept by survey
  for (s in 1:S) {
    re_survey[s] ~ normal(0, sd_re_survey[lookup_cnt[lookup_svy[s]]]);
  }  

  // Temporal processes
  a_t[1] ~ normal(mu, sd_t/ sqrt(1 - phi^2));
  for (y in 2:Y) {
    a_t[y] ~  normal(mu + mu_d * (y-1) + phi * (a_t[y-1] - mu - mu_d * (y-2)), sd_t);
  }

  // Random intercept by country nested in regions
  for(c in 1:C){
    a_c[c] ~ normal(a_r[lookup_cnt[c]], sd_re_cnt[lookup_cnt[c]]);
    delta_cnt[c] ~ normal(delta_r[lookup_cnt[c]], sd_rs_cnt[lookup_cnt[c]]);
  }

  // Random intercept by regions centered in 0
  for(r in 1:R){
    a_r[r] ~ normal(0, sd_re_region);
    delta_r[r] ~ normal(0, sd_rs_region);
  }
    num ~ binomial_logit(den, a_c[country_idx] + delta_cnt[country_idx] .* time +
                                  a_t[year_idx] + fixed_cov + re_survey[survey_idx]);
} 

