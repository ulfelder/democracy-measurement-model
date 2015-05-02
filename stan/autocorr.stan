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
  real<lower=-2,upper=2> expert_bias[num_experts];

  // noise with which an expert observes democracy
  real<lower=0.1,upper=3> expert_var[num_experts];

  // a big matrix encoding our ground truth democracy measure
  real democracy[num_countries,num_years];

  // variance across countries for y=1
  real<lower=0,upper=20> country_var;

  // variance year-to-year changes for countries (y=2 onward)
  real<lower=0,upper=5> time_var;
}

model {
  // draw the true values for democracy for each country/year
  for (c in 1:num_countries) {
    // first year
    democracy[c,1] ~ normal(0, country_var);

    // subsequent years are a random walk
    // use cauchy to allow higher probability of big jumps
    for (y in 2:num_years) {
      democracy[c,y] ~ cauchy(democracy[c,y-1], time_var);
    }
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

