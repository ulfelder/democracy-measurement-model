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
  real<lower=-1,upper=1> expert_bias[num_experts];

  // noise with which an expert observes democracy
  real<lower=0.5,upper=10> expert_var[num_experts];

  // a big matrix encoding our ground truth democracy measure
  real democracy[num_countries,num_years];

  // country-level means
  real country_mean[num_countries];
  real<lower=0> country_var[num_countries];
}

model {
  // draw the true values for democracy for each country/year
  for (c in 1:num_countries) {

    democracy[c] ~ normal(country_mean[c], country_var[c]);

  }

  // now draw experts observations
  for (i in 1:num_obs) {
  
    // expert signal = (truth / variance) + bias
    labels[i] ~ bernoulli_logit(
      (democracy[country[i], year[i]] / expert_var[expert[i]])
      + expert_bias[expert[i]]
    );
  }
}

