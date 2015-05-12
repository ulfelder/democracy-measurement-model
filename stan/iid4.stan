data {
  int<lower=1> num_experts;
  int<lower=1> num_obs;
  int<lower=1> num_countries;
  int<lower=1> num_years;

  // what the experts have said
  int<lower=0,upper=1> labels[num_obs];

  // country for observation n
  int<lower=1,upper=num_countries> country[num_obs];

  // year for observation n
  int<lower=1,upper=num_years> year[num_obs];

  // expert for observation n
  int<lower=1,upper=num_experts> expert[num_obs];
}

parameters {

  // encodes expert bias
  // <0 underpredicts democracy
  // >0 overpredicts democracy
  real expert_bias[num_experts];

  // noise with which an expert observes democracy
  real<lower=0> expert_var[num_experts];

  // a big matrix encoding our ground truth democracy measure
  real<lower=-10,upper=10> democracy[num_countries,num_years];
}

transformed parameters {
  vector[num_obs] expert_bias_vec;
  vector[num_obs] expert_var_vec;
  vector[num_obs] democracy_vec;

  for (i in 1:num_obs) {
    expert_bias_vec[i] <- expert_bias[expert[i]];
    expert_var_vec[i] <- expert_var[expert[i]];
    democracy_vec[i] <- democracy[country[i], year[i]];
  }
}

model {
  // draw expert bias and variances
  expert_bias ~ normal(0, 0.25);
  expert_var ~ chi_square(1);

  labels ~ bernoulli_logit((democracy_vec + expert_bias_vec)
                           ./ expert_var_vec);
}
